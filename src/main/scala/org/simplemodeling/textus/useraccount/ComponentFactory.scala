package org.simplemodeling.textus.useraccount

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.concurrent.TrieMap

import cats.syntax.all.*
import org.goldenport.Consequence
import org.goldenport.datatype.Name
import org.simplemodeling.model.datatype.EntityId
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.record.Record
import org.goldenport.cncf.action.ActionCall
import org.goldenport.cncf.component.{Component, ComponentCreate, EntityRuntimePlanProvider}
import org.goldenport.cncf.CncfVersion
import org.goldenport.cncf.context.ExecutionContext
import org.goldenport.cncf.directive.Query
import org.goldenport.cncf.entity.runtime.{EntityMemoryPolicy, EntityRuntimePlan, PartitionStrategy, WorkingSetDefinition}
import org.goldenport.cncf.unitofwork.ExecUowM
import org.simplemodeling.model.directive.{Condition, Update}

import org.simplemodeling.textus.useraccount.entity.{Credential => CredentialEntity, UserAccount => UserAccountEntity}
import org.simplemodeling.textus.useraccount.entity.create.{Credential => CredentialCreate, UserAccount => UserAccountCreate}
import org.simplemodeling.textus.useraccount.entity.query.{Credential => CredentialQuery, UserAccount => UserAccountQuery}
import org.simplemodeling.textus.useraccount.entity.update.{Credential => CredentialUpdate, UserAccount => UserAccountUpdate}

/*
 * @since   Mar. 23, 2026
 *  version Mar. 24, 2026
 * @version Apr.  6, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentFactory extends UserAccountComponent.Factory with EntityRuntimePlanProvider:
  import UserAccountComponent.ManagementService
  import UserAccountComponent.UserService

  override protected def create_Components(params: ComponentCreate): Vector[Component] =
    Vector(UserAccountComponent())

  override val entityRuntimePlans: Vector[EntityRuntimePlan[Any]] =
    Vector(
      EntityRuntimePlan(
        entityName = UserAccountQuery.collectionId.name,
        memoryPolicy = EntityMemoryPolicy.LoadToMemory,
        workingSet = Some(
          WorkingSetDefinition(
            entityName = UserAccountQuery.collectionId.name,
            entities = ComponentFactory.LoginWorkingSet.snapshot
          )
        ),
        partitionStrategy = PartitionStrategy.byOrganizationMonthUTC,
        maxPartitions = 64,
        maxEntitiesPerPartition = 10000
      ),
      EntityRuntimePlan(
        entityName = CredentialQuery.collectionId.name,
        memoryPolicy = EntityMemoryPolicy.StoreOnly,
        workingSet = None,
        partitionStrategy = PartitionStrategy.byOrganizationMonthUTC,
        maxPartitions = 64,
        maxEntitiesPerPartition = 10000
      )
    )

  override val User: UserAccountComponent.UserServiceFactory = new UserAccountComponent.UserServiceFactory:
    override def createProvisionalRegistrationActionCall(
      core: ActionCall.Core,
      action: UserService.ProvisionalRegistrationCommand
    ): UserService.ProvisionalRegistrationActionCall =
      ProvisionalRegistrationActionCallImpl(core, action)

    override def createRegisterActionCall(
      core: ActionCall.Core,
      action: UserService.RegisterCommand
    ): UserService.RegisterActionCall =
      RegisterActionCallImpl(core, action)

    override def createPromoteToFormalRegistrationActionCall(
      core: ActionCall.Core,
      action: UserService.PromoteToFormalRegistrationCommand
    ): UserService.PromoteToFormalRegistrationActionCall =
      PromoteToFormalRegistrationActionCallImpl(core, action)

    override def createLoginActionCall(
      core: ActionCall.Core,
      action: UserService.LoginCommand
    ): UserService.LoginActionCall =
      LoginActionCallImpl(core, action)

    override def createLogoutActionCall(
      core: ActionCall.Core,
      action: UserService.LogoutCommand
    ): UserService.LogoutActionCall =
      LogoutActionCallImpl(core, action)

    override def createChangePasswordActionCall(
      core: ActionCall.Core,
      action: UserService.ChangePasswordCommand
    ): UserService.ChangePasswordActionCall =
      ChangePasswordActionCallImpl(core, action)

    override def createGetMyAccountActionCall(
      core: ActionCall.Core,
      action: UserService.GetMyAccountQuery
    ): UserService.GetMyAccountActionCall =
      GetMyAccountActionCallImpl(core, action)

  override val Management: UserAccountComponent.ManagementServiceFactory = new UserAccountComponent.ManagementServiceFactory:
    override def createCreateUserAccountActionCall(
      core: ActionCall.Core,
      action: ManagementService.CreateUserAccountCommand
    ): ManagementService.CreateUserAccountActionCall =
      ManagementCreateUserAccountActionCallImpl(core, action)

    override def createUpdateUserStatusActionCall(
      core: ActionCall.Core,
      action: ManagementService.UpdateUserStatusCommand
    ): ManagementService.UpdateUserStatusActionCall =
      ManagementUpdateUserStatusActionCallImpl(core, action)

    override def createDeleteUserAccountActionCall(
      core: ActionCall.Core,
      action: ManagementService.DeleteUserAccountCommand
    ): ManagementService.DeleteUserAccountActionCall =
      ManagementDeleteUserAccountActionCallImpl(core, action)

    override def createListUserAccountsActionCall(
      core: ActionCall.Core,
      action: ManagementService.ListUserAccountsQuery
    ): ManagementService.ListUserAccountsActionCall =
      ManagementListUserAccountsActionCallImpl(core, action)

  private trait UserAccountActionSupport:
    self: org.goldenport.cncf.action.ActionCall =>

    protected final def createUserAndCredential(
      record: Record,
      defaultStatus: Option[String]
    ): ExecUowM[OperationResponse] =
      for
        password <- exec_from(requiredString(record, List("password")))
        userRecord = withDefaultStatus(record, defaultStatus)
        status <- exec_from(requiredStatus(userRecord))
        _ <- exec_from(ComponentFactory.requireCreatableStatus(status))
        user <- exec_from(UserAccountCreate.createWithExecutionContextC(userRecord)(using executionContext))
        created <- entity_create(user)
        credentialRecord = Record.dataAuto(
          "userAccountId" -> created.id,
          "passwordHash" -> passwordHash(password)
        )
        credential <- exec_from(CredentialCreate.createWithExecutionContextC(credentialRecord)(using executionContext))
        _ <- entity_create(credential)
      yield
        OperationResponse(created.toRecord)

    protected final def updateUserStatus(
      record: Record,
      forcedStatus: Option[String]
    ): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        user <- entity_load[UserAccountEntity](userId)
        targetStatus <- forcedStatus match
          case Some(s) => exec_from(ComponentFactory.parseStatus(s))
          case None => exec_from(requiredStatus(record))
        _ <- exec_from(ComponentFactory.requireTransition(user.status, targetStatus))
        patchRecord = Record.dataAuto("status" -> targetStatus.value)
        patch <- exec_from(UserAccountUpdate.createC(patchRecord))
        _ <- entity_update(userId, patch)
      yield
        OperationResponse.void

    protected final def deleteUserAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        credentials <- credentialsForUser(userId)
        _ <- deleteCredentials(credentials)
        _ <- exec_from(removeLoggedInUser(userId))
        _ <- entity_delete(userId)
      yield
        OperationResponse.void

    protected final def listUserAccounts(record: Record): ExecUowM[OperationResponse] =
      for
        status <- exec_from(optionalStatus(record))
        email <- exec_from(optionalString(record, List("email")))
        offset <- exec_from(optionalInt(record, List("offset")))
        limit <- exec_from(optionalInt(record, List("limit")))
        condition = UserAccountQuery(
          id = Condition.any[EntityId],
          name = Condition.any[Name],
          title = Condition.any[String],
          email = email.map(Condition.is[String]).getOrElse(Condition.any[String]),
          status = status.map(Condition.is[UserAccountStatus]).getOrElse(Condition.any[UserAccountStatus])
        )
        query = Query.plan(condition, limit = limit, offset = offset)
        result <- entity_search[UserAccountEntity](UserAccountQuery.collectionId, query)
      yield
        OperationResponse.create(result)

    protected final def login(record: Record): ExecUowM[OperationResponse] =
      for
        email <- exec_from(requiredString(record, List("email")))
        password <- exec_from(requiredString(record, List("password")))
        user <- userByEmail(email)
        _ <- exec_from(ComponentFactory.requireAuthenticatable(user.status))
        credential <- credentialByUserAndPassword(user.id, password)
        _ <- exec_from(addLoggedInUser(user))
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "credentialId" -> credential.id.print,
            "authenticated" -> true
          )
        )

    protected final def logout(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        _ <- exec_from(ComponentFactory.requireLoggedIn(userId))
        _ <- exec_from(removeLoggedInUser(userId))
      yield
        OperationResponse.void

    protected final def changePassword(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        current <- exec_from(requiredString(record, List("currentPassword", "current_password")))
        next <- exec_from(requiredString(record, List("newPassword", "new_password")))
        credential <- credentialByUserAndPassword(userId, current)
        patch <- exec_from(CredentialUpdate.createC(Record.dataAuto("passwordHash" -> passwordHash(next))))
        _ <- entity_update(credential.id, patch)
      yield
        OperationResponse.void

    protected final def getMyAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(currentUserId(record))
        user <- entity_load[UserAccountEntity](userId)
      yield
        OperationResponse(user.toRecord())

    protected final def requireManagementPrivilege(): Consequence[Unit] =
      ComponentFactory.requireManagementPrivilege(using executionContext)

    private def withDefaultStatus(record: Record, defaultStatus: Option[String]): Record =
      defaultStatus match
        case Some(status) if !hasStringValue(record, List("status")) =>
          record ++ Record.data("status" -> status)
        case _ =>
          record

    private def userByEmail(email: String): ExecUowM[UserAccountEntity] =
      val query = UserAccountQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        email = Condition.is(email),
        status = Condition.any[UserAccountStatus]
      )
      for
        result <- entity_search[UserAccountEntity](UserAccountQuery.collectionId, Query(query))
        user <- exec_from(firstOrFailure(result.data, s"user account not found by email: $email"))
      yield
        user

    private def credentialsForUser(userId: EntityId): ExecUowM[Vector[CredentialEntity]] =
      val query = CredentialQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userId),
        passwordHash = Condition.any[String]
      )
      entity_search[CredentialEntity](CredentialQuery.collectionId, Query(query)).map(_.data)

    private def credentialByUserAndPassword(
      userId: EntityId,
      password: String
    ): ExecUowM[CredentialEntity] =
      val query = CredentialQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userId),
        passwordHash = Condition.is(passwordHash(password))
      )
      for
        result <- entity_search[CredentialEntity](CredentialQuery.collectionId, Query(query))
        credential <- exec_from(firstOrFailure(result.data, "invalid credentials"))
      yield
        credential

    private def deleteCredentials(credentials: Vector[CredentialEntity]): ExecUowM[Unit] =
      credentials.foldLeft(exec_pure(())) { (z, credential) =>
        z.flatMap(_ => entity_delete(credential.id))
      }

    private def passwordHash(password: String): String =
      val digest = MessageDigest.getInstance("SHA-256")
      val bytes = digest.digest(password.getBytes(StandardCharsets.UTF_8))
      bytes.map(b => f"${b & 0xff}%02x").mkString

    private def addLoggedInUser(user: UserAccountEntity): Consequence[Unit] =
      ComponentFactory.LoginWorkingSet.upsert(user)
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.put(user))
      Consequence.unit

    private def removeLoggedInUser(userId: EntityId): Consequence[Unit] =
      ComponentFactory.LoginWorkingSet.remove(userId)
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.remove(userId))
      Consequence.unit

    protected final def requiredString(record: Record, keys: List[String]): Consequence[String] =
      recordGetAsC[String](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def requiredEntityId(record: Record, keys: List[String]): Consequence[EntityId] =
      recordGetAsC[EntityId](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def optionalString(record: Record, keys: List[String]): Consequence[Option[String]] =
      recordGetAsC[String](record, keys)

    private def optionalInt(record: Record, keys: List[String]): Consequence[Option[Int]] =
      recordGetAsC[Int](record, keys)

    private def requiredStatus(record: Record): Consequence[UserAccountStatus] =
      recordGetAsC[UserAccountStatus](record, List("status")).flatMap(v => Consequence.successOrPropertyNotFound("status", v))

    private def optionalStatus(record: Record): Consequence[Option[UserAccountStatus]] =
      recordGetAsC[UserAccountStatus](record, List("status"))

    private def currentUserId(record: Record): Consequence[EntityId] =
      recordGetAsC[EntityId](record, List("userAccountId", "user_account_id", "id")).flatMap {
        case Some(id) => ComponentFactory.currentLoggedInUserId(id)
        case None => ComponentFactory.currentLoggedInUserId()
      }

    private def hasStringValue(record: Record, keys: List[String]): Boolean =
      recordGetAsC[String](record, keys) match
        case Consequence.Success(Some(_)) => true
        case _ => false

    private def firstOrFailure[A](xs: Vector[A], message: String): Consequence[A] =
      xs.headOption match
        case Some(s) => Consequence.success(s)
        case None => Consequence.failure(message)

    private def recordGetAsC[A](
      record: Record,
      keys: List[String]
    )(using vr: org.goldenport.convert.ValueReader[A]): Consequence[Option[A]] =
      keys.foldLeft(Consequence.success(Option.empty[A])) { (z, key) =>
        z.flatMap {
          case s @ Some(_) => Consequence.success(s)
          case None => record.getAsC[A](key)
        }
      }

  private final case class ProvisionalRegistrationActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.ProvisionalRegistrationCommand
  ) extends UserService.ProvisionalRegistrationActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      createUserAndCredential(action.record, Some("provisional"))

  private final case class RegisterActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.RegisterCommand
  ) extends UserService.RegisterActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      createUserAndCredential(action.record, Some("registered"))

  private final case class PromoteToFormalRegistrationActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.PromoteToFormalRegistrationCommand
  ) extends UserService.PromoteToFormalRegistrationActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        proofToken <- exec_from(requiredString(action.record, List("proofToken", "proof_token")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        response <- updateUserStatus(action.record, Some("formal"))
      yield
        response

  private final case class LoginActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.LoginCommand
  ) extends UserService.LoginActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      login(action.record)

  private final case class LogoutActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.LogoutCommand
  ) extends UserService.LogoutActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      logout(action.record)

  private final case class ChangePasswordActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.ChangePasswordCommand
  ) extends UserService.ChangePasswordActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      changePassword(action.record)

  private final case class GetMyAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.GetMyAccountQuery
  ) extends UserService.GetMyAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      getMyAccount(action.record)

  private final case class ManagementCreateUserAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.CreateUserAccountCommand
  ) extends ManagementService.CreateUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- createUserAndCredential(action.record, Some("formal"))
      yield
        response

  private final case class ManagementUpdateUserStatusActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.UpdateUserStatusCommand
  ) extends ManagementService.UpdateUserStatusActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- updateUserStatus(action.record, None)
      yield
        response

  private final case class ManagementDeleteUserAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.DeleteUserAccountCommand
  ) extends ManagementService.DeleteUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- deleteUserAccount(action.record)
      yield
        response

  private final case class ManagementListUserAccountsActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.ListUserAccountsQuery
  ) extends ManagementService.ListUserAccountsActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- listUserAccounts(action.record)
      yield
        response

object ComponentFactory:
  private val _management_capabilities = Set(
    "content_manager",
    "content_admin",
    "application_content_manager"
  )

  private val _creatable_statuses: Set[UserAccountStatus] = Set(
    UserAccountStatus.Provisional,
    UserAccountStatus.Registered,
    UserAccountStatus.Formal,
    UserAccountStatus.Suspended
  )

  private val _authenticatable_statuses: Set[UserAccountStatus] = Set(
    UserAccountStatus.Registered,
    UserAccountStatus.Formal
  )

  private val _transitions = Map[UserAccountStatus, Set[UserAccountStatus]](
    UserAccountStatus.Provisional -> Set(UserAccountStatus.Formal, UserAccountStatus.Suspended),
    UserAccountStatus.Registered -> Set(UserAccountStatus.Suspended),
    UserAccountStatus.Formal -> Set(UserAccountStatus.Suspended),
    UserAccountStatus.Suspended -> Set(UserAccountStatus.Registered)
  )

  def parseStatus(p: String): Consequence[UserAccountStatus] =
    UserAccountStatus.parse(p)

  def requireCreatableStatus(p: UserAccountStatus): Consequence[Unit] =
    if (_creatable_statuses.contains(p))
      Consequence.unit
    else
      Consequence.failure(s"Status ${p.value} is not allowed for account creation.")

  def requireAuthenticatable(p: UserAccountStatus): Consequence[Unit] =
    if (_authenticatable_statuses.contains(p))
      Consequence.unit
    else
      Consequence.failure(s"Status ${p.value} cannot authenticate.")

  def requireTransition(current: UserAccountStatus, target: UserAccountStatus): Consequence[Unit] =
    if (current == target)
      Consequence.unit
    else if (_transitions.getOrElse(current, Set.empty).contains(target))
      Consequence.unit
    else
      Consequence.failure(s"Status transition ${current.value} -> ${target.value} is not allowed.")

  def requirePromotionProofToken(token: String): Consequence[Unit] =
    if (token.trim.nonEmpty)
      Consequence.unit
    else
      Consequence.failure("Promotion proof token must not be empty.")

  def requireLoggedIn(id: EntityId): Consequence[Unit] =
    if (LoginWorkingSet.contains(id))
      Consequence.unit
    else
      Consequence.failure(s"User account is not active in working set: ${id.print}")

  def requireManagementPrivilege(using ctx: ExecutionContext): Consequence[Unit] =
    if (ctx.security.hasAnyCapability(_management_capabilities))
      Consequence.unit
    else
      Consequence.failure("Management privilege is required.")

  def currentLoggedInUserId(): Consequence[EntityId] =
    LoginWorkingSet.currentUserId()

  def currentLoggedInUserId(id: EntityId): Consequence[EntityId] =
    requireLoggedIn(id).map(_ => id)

  private object LoginWorkingSet:
    private val users = TrieMap.empty[EntityId, UserAccountEntity]

    def upsert(user: UserAccountEntity): Unit =
      users.put(user.id, user)

    def remove(id: EntityId): Unit =
      users.remove(id)

    def contains(id: EntityId): Boolean =
      users.contains(id)

    def currentUserId(): Consequence[EntityId] =
      users.keys.toVector match
        case Vector(id) => Consequence.success(id)
        case Vector() => Consequence.failure("No active user account is available in working set.")
        case _ => Consequence.failure("Multiple active user accounts are available in working set.")

    def snapshot: Vector[UserAccountEntity] =
      users.values.toVector

  def create(componentCreate: ComponentCreate): Vector[Component] =
    new ComponentFactory().create(componentCreate).map(_withArtifactMetadata)

  def createStandalone(): Component =
    _withArtifactMetadata(UserAccountComponent())

  private def _withArtifactMetadata(component: Component): Component =
    component.withArtifactMetadata(
      Component.ArtifactMetadata(
        sourceType = "standalone",
        name = "textus-user-account",
        version = CncfVersion.current,
        component = Some("textus-user-account")
      )
    )
