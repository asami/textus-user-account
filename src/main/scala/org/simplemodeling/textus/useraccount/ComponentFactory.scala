package org.simplemodeling.textus.useraccount

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.UUID
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
import org.goldenport.cncf.context.{Capability, ExecutionContext, PrincipalId, SecurityLevel, SessionContext, SubjectKind}
import org.goldenport.cncf.datastore.{Query as DsQuery, QueryDirective as DsQueryDirective}
import org.goldenport.cncf.directive.Query
import org.goldenport.cncf.entity.EntityStore
import org.goldenport.cncf.entity.runtime.{EntityMemoryPolicy, EntityRuntimePlan, PartitionStrategy, WorkingSetDefinition}
import org.goldenport.cncf.operation.CmlOperationAccess
import org.goldenport.cncf.security.{AuthenticationProvider, AuthenticationRequest, AuthenticationResult, OperationAccessPolicy}
import org.goldenport.cncf.unitofwork.{ExecUowM, UnitOfWorkAuthorization}
import org.simplemodeling.model.directive.{Condition, Update}
import org.simplemodeling.model.statemachine.ActivationStatus
import org.simplemodeling.model.value.{ResourceAttributes, ResourceAttributesUpdate}

import org.simplemodeling.textus.useraccount.entity.{AccessSession => AccessSessionEntity, Credential => CredentialEntity, RefreshSession => RefreshSessionEntity, UserAccount => UserAccountEntity}
import org.simplemodeling.textus.useraccount.entity.create.{AccessSession => AccessSessionCreate, Credential => CredentialCreate, RefreshSession => RefreshSessionCreate, UserAccount => UserAccountCreate}
import org.simplemodeling.textus.useraccount.entity.query.{AccessSession => AccessSessionQuery, Credential => CredentialQuery, RefreshSession => RefreshSessionQuery, UserAccount => UserAccountQuery}
import org.simplemodeling.textus.useraccount.entity.update.{AccessSession => AccessSessionUpdate, Credential => CredentialUpdate, RefreshSession => RefreshSessionUpdate, UserAccount => UserAccountUpdate}

/*
 * @since   Mar. 23, 2026
 *  version Mar. 24, 2026
 * @version Apr.  9, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentFactory extends UserAccountComponent.Factory with EntityRuntimePlanProvider:
  import UserAccountComponent.ManagementService
  import UserAccountComponent.UserService

  override def authorize_operation_access(
    action: org.goldenport.cncf.action.Action,
    access: CmlOperationAccess,
    core: ActionCall.Core
  ): Option[Consequence[Unit]] =
    access.policy.trim.toLowerCase(java.util.Locale.ROOT) match
      case "owner_or_manager" | "owner-or-manager" =>
        Some(ComponentFactory.authorizeOwnerOrManagerUserAccount(action.request.toRecord, access)(using core.executionContext))
      case "manager_only" | "manager-only" =>
        Some(OperationAccessPolicy.authorizeManagerOnly()(using core.executionContext))
      case _ => None

  override def authorize_operation_entity(
    action: org.goldenport.cncf.action.Action,
    entityName: String,
    core: ActionCall.Core
  ): Option[Consequence[Unit]] =
    entityName.trim.toLowerCase(java.util.Locale.ROOT) match
      case "useraccount" | "user_account" =>
        Some(ComponentFactory.authorizeUserAccountEntityOperation(action.name, action.request.toRecord, entityName)(using core.executionContext))
      case _ => None

  override def authorize_unit_of_work(
    authorization: UnitOfWorkAuthorization,
    uow: org.goldenport.cncf.unitofwork.UnitOfWork
  ): Option[Consequence[Unit]] =
    None

  override protected def create_Components(params: ComponentCreate): Vector[Component] =
    Vector(
      new UserAccountComponent() {
        override def authenticationProviders: Vector[AuthenticationProvider] =
          Vector(ComponentFactory.UserAccountAuthenticationProvider)
      }
    )

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
      ),
      EntityRuntimePlan(
        entityName = AccessSessionQuery.collectionId.name,
        memoryPolicy = EntityMemoryPolicy.StoreOnly,
        workingSet = None,
        partitionStrategy = PartitionStrategy.byOrganizationMonthUTC,
        maxPartitions = 64,
        maxEntitiesPerPartition = 10000
      ),
      EntityRuntimePlan(
        entityName = RefreshSessionQuery.collectionId.name,
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

    override def createRefreshAccessTokenActionCall(
      core: ActionCall.Core,
      action: UserService.RefreshAccessTokenCommand
    ): UserService.RefreshAccessTokenActionCall =
      RefreshAccessTokenActionCallImpl(core, action)

    override def createChangePasswordActionCall(
      core: ActionCall.Core,
      action: UserService.ChangePasswordCommand
    ): UserService.ChangePasswordActionCall =
      ChangePasswordActionCallImpl(core, action)

    override def createVerifyMyEmailActionCall(
      core: ActionCall.Core,
      action: UserService.VerifyMyEmailCommand
    ): UserService.VerifyMyEmailActionCall =
      VerifyMyEmailActionCallImpl(core, action)

    override def createVerifyMyPhoneActionCall(
      core: ActionCall.Core,
      action: UserService.VerifyMyPhoneCommand
    ): UserService.VerifyMyPhoneActionCall =
      VerifyMyPhoneActionCallImpl(core, action)

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
        email <- exec_from(requiredString(userRecord, List("email")))
        _ <- requireEmailAvailable(email)
        status <- exec_from(requiredStatus(userRecord))
        _ <- exec_from(ComponentFactory.requireCreatableStatus(status))
        user0 <- exec_from(UserAccountCreate.createWithExecutionContextC(userRecord)(using executionContext))
        user = user0.withResourceAttributes(ResourceAttributes(activationStatus = ComponentFactory.activationStatusForUserAccountStatus(status)))
        created <- entity_create(user)
        credentialRecord = Record.dataAuto(
          "userAccountId" -> created.id,
          "passwordHash" -> passwordHash(password)
        )
        credential0 <- exec_from(CredentialCreate.createWithExecutionContextC(credentialRecord)(using executionContext))
        credential = credential0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
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
        suspensionReason <- exec_from(optionalString(record, List("suspensionReason", "suspension_reason")))
        _ <- exec_from(ComponentFactory.requireTransition(user.status, targetStatus))
        _ <- _update_user_account_fields(
          userId,
          ComponentFactory.statusUpdateFields(targetStatus, suspensionReason)(using executionContext)
        )
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
          emailVerifiedAt = Condition.any[String],
          phoneNumber = Condition.any[String],
          phoneVerifiedAt = Condition.any[String],
          lastLoginAt = Condition.any[String],
          passwordChangedAt = Condition.any[String],
          suspendedAt = Condition.any[String],
          suspendedBy = Condition.any[String],
          suspensionReason = Condition.any[String],
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
        refreshIssued <- issueRefreshSession(
          user.id,
          refreshTokenFromSecurityContext(),
          clientIdFromSecurityContext(),
          deviceInfoFromSecurityContext(),
          ipAddressFromSecurityContext(),
          userAgentFromSecurityContext()
        )
        issued <- issueAccessSession(
          user.id,
          accessTokenFromSecurityContext(),
          Some(refreshIssued.sessionId),
          clientIdFromSecurityContext(),
          deviceInfoFromSecurityContext(),
          ipAddressFromSecurityContext(),
          userAgentFromSecurityContext()
        )
        _ <- updateLastLoginAt(user.id)
        _ <- exec_from(addLoggedInUser(user))
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "credentialId" -> credential.id.print,
            "accessSessionId" -> issued.sessionId.print,
            "accessToken" -> issued.rawToken,
            "refreshSessionId" -> refreshIssued.sessionId.print,
            "refreshToken" -> refreshIssued.rawToken,
            "authenticated" -> true
          )
        )

    protected final def logout(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        _ <- requireUserSession(userId)
        _ <- revokeCurrentAccessSession(userId)
        _ <- revokeCurrentRefreshSession(userId)
        _ <- exec_from(removeLoggedInUser(userId))
      yield
        OperationResponse.void

    protected final def refreshAccessToken(record: Record): ExecUowM[OperationResponse] =
      for
        refreshToken <- exec_from(requiredString(record, List("refreshToken", "refresh_token")))
        refreshSession <- activeRefreshSessionByToken(refreshToken)
        _ <- revokeRefreshSessionAsRotated(refreshSession.id)
        refreshIssued <- issueRefreshSession(
          refreshSession.userAccountId,
          None,
          clientIdFromSecurityContext().orElse(refreshSession.clientId),
          deviceInfoFromSecurityContext().orElse(refreshSession.deviceInfo),
          ipAddressFromSecurityContext().orElse(refreshSession.ipAddress),
          userAgentFromSecurityContext().orElse(refreshSession.userAgent),
          predecessor = Some(refreshSession.id)
        )
        accessIssued <- issueAccessSession(
          refreshSession.userAccountId,
          None,
          Some(refreshIssued.sessionId),
          clientIdFromSecurityContext().orElse(refreshSession.clientId),
          deviceInfoFromSecurityContext().orElse(refreshSession.deviceInfo),
          ipAddressFromSecurityContext().orElse(refreshSession.ipAddress),
          userAgentFromSecurityContext().orElse(refreshSession.userAgent)
        )
        _ <- exec_from(addLoggedInUserForUserId(refreshSession.userAccountId))
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> refreshSession.userAccountId.print,
            "accessSessionId" -> accessIssued.sessionId.print,
            "accessToken" -> accessIssued.rawToken,
            "refreshSessionId" -> refreshIssued.sessionId.print,
            "refreshToken" -> refreshIssued.rawToken,
            "authenticated" -> true
          )
        )

    protected final def changePassword(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        current <- exec_from(requiredString(record, List("currentPassword", "current_password")))
        next <- exec_from(requiredString(record, List("newPassword", "new_password")))
        credential <- credentialByUserAndPassword(userId, current)
        patch <- exec_from(CredentialUpdate.createC(Record.dataAuto("passwordHash" -> passwordHash(next))))
        _ <- entity_update(credential.id, patch)
        _ <- updatePasswordChangedAt(userId)
        _ <- revokeAllAccessSessions(userId)
        _ <- revokeAllRefreshSessions(userId)
      yield
        OperationResponse.void

    protected final def getMyAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(currentUserId(record))
        user <- entity_load[UserAccountEntity](userId)
      yield
        OperationResponse(user.toRecord())

    protected final def verifyMyEmail(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(currentUserId(record))
        user <- rawUserAccountRecord(userId)
        proofToken <- exec_from(requiredString(record, List("proofToken", "proof_token")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        _ <- exec_from(ComponentFactory.requireEmailVerificationPending(user))
        _ <- updateEmailVerifiedAt(userId)
      yield
        OperationResponse.void

    protected final def verifyMyPhone(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(currentUserId(record))
        user <- rawUserAccountRecord(userId)
        phoneNumber <- exec_from(requiredString(record, List("phoneNumber", "phone_number")))
        proofToken <- exec_from(requiredString(record, List("proofToken", "proof_token")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        _ <- exec_from(ComponentFactory.requirePhoneVerificationPending(user, phoneNumber))
        _ <- updatePhoneVerification(userId, phoneNumber)
      yield
        OperationResponse.void

    protected final def requireManagementPrivilege(): Consequence[Unit] =
      ComponentFactory.requireManagementPrivilege(using executionContext)

    private def withDefaultStatus(record: Record, defaultStatus: Option[String]): Record =
      defaultStatus match
        case Some(status) if !hasStringValue(record, List("status")) =>
          record ++ Record.data("status" -> status)
        case _ =>
          record

    private def userByEmail(email: String): ExecUowM[UserAccountEntity] =
      for
        users <- rawUserAccountsByEmail(email)
        user <- exec_from(firstOrFailure(users, s"user account not found by email: $email"))
      yield
        user

    private def requireEmailAvailable(email: String): ExecUowM[Unit] =
      for
        users <- rawUserAccountsByEmail(email)
        _ <- exec_from(ComponentFactory.requireEmailAvailable(email, users))
      yield
        ()

    private def credentialsForUser(userId: EntityId): ExecUowM[Vector[CredentialEntity]] =
      val query = CredentialQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userId),
        passwordHash = Condition.any[String]
      )
      entity_search[CredentialEntity](CredentialQuery.collectionId, Query(query)).map(_.data)

    private def accessSessionsForUser(userId: EntityId): ExecUowM[Vector[AccessSessionEntity]] =
      val query = AccessSessionQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userId),
        refreshSessionId = Condition.any[String],
        tokenHash = Condition.any[String],
        issuedAt = Condition.any[String],
        expiresAt = Condition.any[String],
        revokedAt = Condition.any[String],
        lastAccessedAt = Condition.any[String],
        clientId = Condition.any[String],
        deviceInfo = Condition.any[String],
        ipAddress = Condition.any[String],
        userAgent = Condition.any[String]
      )
      entity_search[AccessSessionEntity](AccessSessionQuery.collectionId, Query(query)).map(_.data)

    private def refreshSessionsForUser(userId: EntityId): ExecUowM[Vector[RefreshSessionEntity]] =
      val query = RefreshSessionQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userId),
        successorSessionId = Condition.any[String],
        tokenHash = Condition.any[String],
        issuedAt = Condition.any[String],
        expiresAt = Condition.any[String],
        revokedAt = Condition.any[String],
        rotatedAt = Condition.any[String],
        clientId = Condition.any[String],
        deviceInfo = Condition.any[String],
        ipAddress = Condition.any[String],
        userAgent = Condition.any[String]
      )
      entity_search[RefreshSessionEntity](RefreshSessionQuery.collectionId, Query(query)).map(_.data)

    private def credentialByUserAndPassword(
      userId: EntityId,
      password: String
    ): ExecUowM[CredentialEntity] =
      for
        credentials <- rawCredentialsByUserAndPassword(userId, passwordHash(password))
        credential <- exec_from(firstOrFailure(credentials, "invalid credentials"))
      yield
        credential

    private def rawUserAccountsByEmail(email: String): ExecUowM[Vector[UserAccountEntity]] =
      for
        records <- rawRecords(UserAccountQuery.collectionId)
        ids <- exec_from(
          records
            .filter(_.getString("email").contains(email))
            .traverse(_entity_id_of)
        )
        users <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[UserAccountEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        users

    private def rawCredentialsByUserAndPassword(
      userId: EntityId,
      hashedPassword: String
    ): ExecUowM[Vector[CredentialEntity]] =
      for
        records <- rawRecords(CredentialQuery.collectionId)
        ids <- exec_from(
          records
            .filter(r =>
              r.getString("user_account_id").contains(userId.print) &&
                r.getString("password_hash").contains(hashedPassword)
            )
            .traverse(_entity_id_of)
        )
        credentials <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[CredentialEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        credentials

    private def rawAccessSessionsByTokenHash(
      hashedToken: String
    ): ExecUowM[Vector[AccessSessionEntity]] =
      for
        records <- rawRecords(AccessSessionQuery.collectionId)
        ids <- exec_from(
          records
            .filter(_.getString("token_hash").contains(hashedToken))
            .traverse(_entity_id_of)
        )
        sessions <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[AccessSessionEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        sessions

    private def rawRefreshSessionsByTokenHash(
      hashedToken: String
    ): ExecUowM[Vector[RefreshSessionEntity]] =
      for
        records <- rawRecords(RefreshSessionQuery.collectionId)
        ids <- exec_from(
          records
            .filter(_.getString("token_hash").contains(hashedToken))
            .traverse(_entity_id_of)
        )
        sessions <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[RefreshSessionEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        sessions

    private def rawAccessSessionsForUser(
      userId: EntityId
    ): ExecUowM[Vector[AccessSessionEntity]] =
      for
        records <- rawRecords(AccessSessionQuery.collectionId)
        ids <- exec_from(
          records
            .filter(_.getString("user_account_id").contains(userId.print))
            .traverse(_entity_id_of)
        )
        sessions <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[AccessSessionEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        sessions

    private def rawRefreshSessionsForUser(
      userId: EntityId
    ): ExecUowM[Vector[RefreshSessionEntity]] =
      for
        records <- rawRecords(RefreshSessionQuery.collectionId)
        ids <- exec_from(
          records
            .filter(_.getString("user_account_id").contains(userId.print))
            .traverse(_entity_id_of)
        )
        sessions <- exec_from(
          ids.traverse(id =>
            EntityStore
              .standard()
              .load[RefreshSessionEntity](id)(using summon, executionContext)
              .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
          )
        )
      yield
        sessions

    private def rawUserAccountRecord(userId: EntityId): ExecUowM[Record] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          recordOption <- ds.load(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(userId)
          )(using executionContext)
          record <- recordOption match
            case Some(x) => Consequence.success(x)
            case None => Consequence.failure(s"User account not found: ${userId.print}")
        yield
          record
      }

    private def rawRecords(collectionId: org.simplemodeling.model.datatype.EntityCollectionId): ExecUowM[Vector[Record]] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionId)
          result <- executionContext.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
        yield
          result.records.toVector
      }

    private def _entity_id_of(record: Record): Consequence[EntityId] =
      record
        .getAsC[EntityId]("id")
        .flatMap(x => Consequence.successOrPropertyNotFound("id", x))

    private def deleteCredentials(credentials: Vector[CredentialEntity]): ExecUowM[Unit] =
      credentials.foldLeft(exec_pure(())) { (z, credential) =>
        z.flatMap(_ => entity_delete(credential.id))
      }

    private def revokeAllAccessSessions(userId: EntityId): ExecUowM[Unit] =
      for
        sessions <- rawAccessSessionsForUser(userId)
        _ <- sessions.foldLeft(exec_pure(())) { (z, session) =>
          z.flatMap(_ => revokeAccessSession(session.id))
        }
      yield
        ()

    private def revokeAllRefreshSessions(userId: EntityId): ExecUowM[Unit] =
      for
        sessions <- rawRefreshSessionsForUser(userId)
        _ <- sessions.foldLeft(exec_pure(())) { (z, session) =>
          z.flatMap(_ => revokeRefreshSession(session.id))
        }
      yield
        ()

    private def issueAccessSession(
      userId: EntityId,
      requestedToken: Option[String],
      refreshSessionId: Option[EntityId],
      clientId: Option[String],
      deviceInfo: Option[String],
      ipAddress: Option[String],
      userAgent: Option[String]
    ): ExecUowM[ComponentFactory.IssuedAccessSession] =
      for
        now <- exec_pure(Instant.now)
        rawToken <- exec_pure(requestedToken.filter(_.trim.nonEmpty).getOrElse(ComponentFactory.generateToken()))
        sessionRecord = Record.dataAuto(
          "userAccountId" -> userId,
          "refreshSessionId" -> refreshSessionId.map(_.print),
          "tokenHash" -> tokenHash(rawToken),
          "issuedAt" -> now.toString,
          "expiresAt" -> now.plusSeconds(ComponentFactory.AccessSessionTtlSeconds.toLong).toString,
          "clientId" -> clientId,
          "deviceInfo" -> deviceInfo,
          "ipAddress" -> ipAddress,
          "userAgent" -> userAgent
        )
        session0 <- exec_from(AccessSessionCreate.createWithExecutionContextC(sessionRecord)(using executionContext))
        session = session0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
        created <- entity_create(session)
      yield
        ComponentFactory.IssuedAccessSession(created.id, rawToken)

    private def issueRefreshSession(
      userId: EntityId,
      requestedToken: Option[String],
      clientId: Option[String],
      deviceInfo: Option[String],
      ipAddress: Option[String],
      userAgent: Option[String],
      predecessor: Option[EntityId] = None
    ): ExecUowM[ComponentFactory.IssuedRefreshSession] =
      for
        now <- exec_pure(Instant.now)
        rawToken <- exec_pure(requestedToken.filter(_.trim.nonEmpty).getOrElse(ComponentFactory.generateToken()))
        sessionRecord = Record.dataAuto(
          "userAccountId" -> userId,
          "tokenHash" -> tokenHash(rawToken),
          "issuedAt" -> now.toString,
          "expiresAt" -> now.plusSeconds(ComponentFactory.RefreshSessionTtlSeconds.toLong).toString,
          "clientId" -> clientId,
          "deviceInfo" -> deviceInfo,
          "ipAddress" -> ipAddress,
          "userAgent" -> userAgent
        )
        session0 <- exec_from(RefreshSessionCreate.createWithExecutionContextC(sessionRecord)(using executionContext))
        session = session0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
        created <- entity_create(session)
        _ <- predecessor match
          case Some(previousId) => updateRefreshSuccessor(previousId, created.id)
          case None => exec_pure(())
      yield
        ComponentFactory.IssuedRefreshSession(created.id, rawToken)

    private def revokeCurrentAccessSession(userId: EntityId): ExecUowM[Unit] =
      currentAccessToken() match
        case Some(token) =>
          for
            sessions <- rawAccessSessionsByTokenHash(tokenHash(token))
            current = sessions.find(_.userAccountId == userId)
            _ <- current match
              case Some(session) => revokeAccessSession(session.id)
              case None => revokeAllAccessSessions(userId)
          yield
            ()
        case None =>
          revokeAllAccessSessions(userId)

    private def revokeCurrentRefreshSession(userId: EntityId): ExecUowM[Unit] =
      currentRefreshToken() match
        case Some(token) =>
          for
            sessions <- rawRefreshSessionsByTokenHash(tokenHash(token))
            current = sessions.find(_.userAccountId == userId)
            _ <- current match
              case Some(session) => revokeRefreshSession(session.id)
              case None => revokeAllRefreshSessions(userId)
          yield
            ()
        case None =>
          revokeAllRefreshSessions(userId)

    private def revokeAccessSession(sessionId: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now.toString)
        _ <- _update_access_session_fields_direct(
          sessionId,
          Record.dataAuto(
            "revoked_at" -> now,
            "last_accessed_at" -> now
          )
        )
      yield
        ()

    private def revokeRefreshSession(sessionId: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now.toString)
        _ <- _update_refresh_session_fields_direct(
          sessionId,
          Record.dataAuto("revoked_at" -> now)
        )
      yield
        ()

    private def revokeRefreshSessionAsRotated(sessionId: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now.toString)
        _ <- _update_refresh_session_fields_direct(
          sessionId,
          Record.dataAuto(
            "revoked_at" -> now,
            "rotated_at" -> now
          )
        )
      yield
        ()

    private def updateRefreshSuccessor(previousId: EntityId, successorId: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_refresh_session_fields_direct(
          previousId,
          Record.dataAuto("successor_session_id" -> successorId.print)
        )
      yield
        ()

    private def requireUserSession(userId: EntityId): ExecUowM[Unit] =
      exec_from(ComponentFactory.requireLoggedIn(userId)(using executionContext))

    private def activeRefreshSessionByToken(refreshToken: String): ExecUowM[RefreshSessionEntity] =
      for
        sessions <- rawRefreshSessionsByTokenHash(tokenHash(refreshToken))
        session <- sessions.find(ComponentFactory.isActiveRefreshSession) match
          case Some(x) =>
            exec_pure(x)
          case None =>
            _handleRefreshTokenReuseOrFailure(sessions)
      yield
        session

    private def _handleRefreshTokenReuseOrFailure(
      sessions: Vector[RefreshSessionEntity]
    ): ExecUowM[RefreshSessionEntity] =
      if (sessions.exists(ComponentFactory._is_reused_refresh_session))
        for
          _ <- sessions.headOption match
            case Some(session) =>
              revokeAllAccessSessions(session.userAccountId).flatMap(_ => revokeAllRefreshSessions(session.userAccountId))
            case None =>
              exec_pure(())
          session <- exec_from(Consequence.failure[RefreshSessionEntity]("refresh token reuse detected"))
        yield
          session
      else
        exec_from(Consequence.failure("invalid refresh token"))

    private def updateLastLoginAt(userId: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userId,
          Record.dataAuto("last_login_at" -> Instant.now.toString)
        )
      yield
        ()

    private def updatePasswordChangedAt(userId: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields(
          userId,
          Record.dataAuto("password_changed_at" -> Instant.now.toString)
        )
      yield
        ()

    private def updateEmailVerifiedAt(userId: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userId,
          Record.dataAuto("email_verified_at" -> Instant.now.toString)
        )
      yield
        ()

    private def updatePhoneVerification(userId: EntityId, phoneNumber: String): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userId,
          Record.dataAuto(
            "phone_number" -> phoneNumber,
            "phone_verified_at" -> Instant.now.toString
          )
        )
      yield
        ()

    private def _update_user_account_fields(
      userId: EntityId,
      changes: Record
    ): ExecUowM[Unit] =
      for
        patch <- exec_from(UserAccountUpdate.createC(changes))
        _ <- entity_update(userId, patch)
      yield
        ()

    private def _update_user_account_fields(
      userId: EntityId,
      patch: UserAccountUpdate
    ): ExecUowM[Unit] =
      entity_update(userId, patch)

    private def _update_user_account_fields_direct(
      userId: EntityId,
      changes: Record
    ): ExecUowM[Unit] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          _ <- ds.update(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(userId),
            changes
          )(using executionContext)
        yield
          ()
      }

    private def _update_refresh_session_fields_direct(
      sessionId: EntityId,
      changes: Record
    ): ExecUowM[Unit] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          _ <- ds.update(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(sessionId),
            changes
          )(using executionContext)
        yield
          ()
      }

    private def _update_access_session_fields_direct(
      sessionId: EntityId,
      changes: Record
    ): ExecUowM[Unit] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          _ <- ds.update(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(sessionId),
            changes
          )(using executionContext)
        yield
          ()
      }

    private def passwordHash(password: String): String =
      tokenHash(password)

    private def tokenHash(token: String): String =
      val digest = MessageDigest.getInstance("SHA-256")
      val bytes = digest.digest(token.getBytes(StandardCharsets.UTF_8))
      bytes.map(b => f"${b & 0xff}%02x").mkString

    private def currentAccessToken(): Option[String] =
      executionContext.security.principal.attributes.get("access_token").filter(_.trim.nonEmpty)

    private def currentRefreshToken(): Option[String] =
      executionContext.security.principal.attributes.get("refresh_token").filter(_.trim.nonEmpty)

    private def accessTokenFromSecurityContext(): Option[String] =
      currentAccessToken()

    private def refreshTokenFromSecurityContext(): Option[String] =
      currentRefreshToken()

    private def clientIdFromSecurityContext(): Option[String] =
      executionContext.security.principal.attributes.get("client_id").filter(_.trim.nonEmpty)

    private def deviceInfoFromSecurityContext(): Option[String] =
      executionContext.security.principal.attributes.get("device_info").filter(_.trim.nonEmpty)

    private def ipAddressFromSecurityContext(): Option[String] =
      executionContext.security.principal.attributes.get("ip_address").filter(_.trim.nonEmpty)

    private def userAgentFromSecurityContext(): Option[String] =
      executionContext.security.principal.attributes.get("user_agent").filter(_.trim.nonEmpty)

    private def addLoggedInUserForUserId(userId: EntityId): Consequence[Unit] =
      ComponentFactory.addLoggedInUserForTest(userId)(using executionContext)

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
        case Some(id) => ComponentFactory.currentLoggedInUserId(id)(using executionContext)
        case None => ComponentFactory.currentLoggedInUserId()(using executionContext)
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

  private final case class RefreshAccessTokenActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.RefreshAccessTokenCommand
  ) extends UserService.RefreshAccessTokenActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      refreshAccessToken(action.record)

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

  private final case class VerifyMyEmailActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.VerifyMyEmailCommand
  ) extends UserService.VerifyMyEmailActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      verifyMyEmail(action.record)

  private final case class VerifyMyPhoneActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.VerifyMyPhoneCommand
  ) extends UserService.VerifyMyPhoneActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      verifyMyPhone(action.record)

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
  private final case class IssuedAccessSession(
    sessionId: EntityId,
    rawToken: String
  )

  private final case class IssuedRefreshSession(
    sessionId: EntityId,
    rawToken: String
  )

  val AccessSessionTtlSeconds: Int = 60 * 60 * 8
  val RefreshSessionTtlSeconds: Int = 60 * 60 * 24 * 14

  def authorizeOwnerOrManagerUserAccount(
    record: Record,
    access: CmlOperationAccess
  )(using ctx: ExecutionContext): Consequence[Unit] =
    for
      userId <- _resolveTargetUserId(record, access)
      _ <- _authorize_owner_or_manager(userId)
    yield
      ()

  def authorizeOwnerOrManagerUserAccountEntity(
    record: Record,
    entityName: String
  )(using ctx: ExecutionContext): Consequence[Unit] =
    authorizeOwnerOrManagerUserAccount(
      record,
      CmlOperationAccess(
        policy = "owner_or_manager",
        resource = Some(entityName),
        target = Some("userAccountId")
      )
    )

  def authorizeUserAccountEntityOperation(
    operationName: String,
    record: Record,
    entityName: String
  )(using ctx: ExecutionContext): Consequence[Unit] =
    operationName match
      case "createUserAccount" | "updateUserStatus" | "deleteUserAccount" | "listUserAccounts" =>
        requireManagementPrivilege
      case _ =>
        authorizeOwnerOrManagerUserAccountEntity(record, entityName)

  private def _requiredEntityId(
    record: Record,
    keys: Seq[String]
  ): Consequence[EntityId] =
    keys.iterator.map(record.getString).collectFirst {
      case Some(s) => summon[org.goldenport.convert.ValueReader[EntityId]].readC(s)
    }.getOrElse {
      Consequence.failure(s"Entity id is required: ${keys.mkString("/")}")
    }

  private def _resolveTargetUserId(
    record: Record,
    access: CmlOperationAccess
  )(using ctx: ExecutionContext): Consequence[EntityId] =
    _requiredEntityId(record, access.target.toList ++ List("userAccountId", "user_account_id", "id")) match
      case s @ Consequence.Success(_) => s
      case _ => currentLoggedInUserId()

  private def _authorize_owner_or_manager(
    userId: EntityId
  )(using ctx: ExecutionContext): Consequence[Unit] =
    OperationAccessPolicy.authorizeSimpleEntityOwnerOrManager(
      userId,
      id => org.goldenport.cncf.entity.EntityStore.standard().load[UserAccountEntity](id).map(_.map(_.toRecord()))
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

  def activationStatusForUserAccountStatus(
    status: UserAccountStatus
  ): ActivationStatus =
    status match
      case UserAccountStatus.Provisional => ActivationStatus.Inactive
      case UserAccountStatus.Registered => ActivationStatus.Active
      case UserAccountStatus.Formal => ActivationStatus.Active
      case UserAccountStatus.Suspended => ActivationStatus.Deactivated

  def statusUpdateFields(
    status: UserAccountStatus,
    suspensionReason: Option[String] = None
  )(using ctx: ExecutionContext): UserAccountUpdate =
    val activationStatus = activationStatusForUserAccountStatus(status)
    status match
      case UserAccountStatus.Suspended =>
        UserAccountUpdate
          .Builder()
          .withStatus(status)
          .withSuspendedAt(Instant.now.toString)
          .withSuspendedBy(ctx.security.principal.id.value)
          .withSuspensionReason(suspensionReason.map(Update.set).getOrElse(Update.noop))
          .withResourceAttributes(
            ResourceAttributesUpdate(
              activationStatus = Update.set(activationStatus)
            )
          )
          .build()
      case _ =>
        UserAccountUpdate
          .Builder()
          .withStatus(status)
          .withSuspendedAt(Update.setNull)
          .withSuspendedBy(Update.setNull)
          .withSuspensionReason(Update.setNull)
          .withResourceAttributes(
            ResourceAttributesUpdate(
              activationStatus = Update.set(activationStatus)
            )
          )
          .build()

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

  def requireEmailVerificationPending(user: Record): Consequence[Unit] =
    user.getString("email_verified_at") match
      case Some(v) if v.trim.nonEmpty => Consequence.failure("Current account email is already verified.")
      case _ => Consequence.unit

  def requirePhoneVerificationPending(
    user: Record,
    requestedPhoneNumber: String
  ): Consequence[Unit] =
    user.getString("phone_number") match
      case None | Some("") =>
        Consequence.failure("Current account does not have an SMS contact to verify.")
      case Some(phone) if phone != requestedPhoneNumber =>
        Consequence.failure(s"Requested phone number does not match the current account SMS contact: $requestedPhoneNumber")
      case Some(_) =>
        user.getString("phone_verified_at") match
          case Some(v) if v.trim.nonEmpty => Consequence.failure("Current account phone number is already verified.")
          case _ => Consequence.unit

  def requireEmailAvailable(
    email: String,
    users: Vector[UserAccountEntity]
  ): Consequence[Unit] =
    if (users.isEmpty)
      Consequence.unit
    else
      Consequence.failure(s"User account email is already registered: $email")

  def requireLoggedIn(id: EntityId)(using ExecutionContext): Consequence[Unit] =
    currentLoggedInUserId(id).map(_ => ())

  def requireManagementPrivilege(using ctx: ExecutionContext): Consequence[Unit] =
    OperationAccessPolicy.authorizeManagerOnly()

  def currentLoggedInUserId()(using ctx: ExecutionContext): Consequence[EntityId] =
    _current_logged_in_user_id_from_access_token().orElse(LoginWorkingSet.currentUserId())

  def currentLoggedInUserId(id: EntityId)(using ctx: ExecutionContext): Consequence[EntityId] =
    currentLoggedInUserId().flatMap { current =>
      if (current == id)
        Consequence.success(id)
      else
        Consequence.failure(s"Current authenticated user does not match target user account: ${id.print}")
    }

  def generateToken(): String =
    UUID.randomUUID().toString.replace("-", "") + UUID.randomUUID().toString.replace("-", "")

  private def _current_logged_in_user_id_from_access_token()(using ctx: ExecutionContext): Consequence[EntityId] =
    ctx.security.principal.attributes.get("access_token").filter(_.trim.nonEmpty) match
      case Some(token) =>
        _active_access_session_by_token(token).map(_.userAccountId)
      case None =>
        Consequence.failure("No access token is available in security context.")

  private def _active_access_session_by_token(
    token: String
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    val hashed = _token_hash(token)
    _raw_access_sessions_by_token_hash(hashed).flatMap { sessions =>
      sessions.find(_is_active_access_session) match
        case Some(session) => Consequence.success(session)
        case None => Consequence.failure("No active access session matches the current access token.")
    }

  private def _raw_access_sessions_by_token_hash(
    hashedToken: String
  )(using ctx: ExecutionContext): Consequence[Vector[AccessSessionEntity]] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      ids <- result.records.toVector
        .filter(_.getString("token_hash").contains(hashedToken))
        .traverse(record =>
          record
            .getAsC[EntityId]("id")
            .flatMap(x => Consequence.successOrPropertyNotFound("id", x))
        )
      sessions <- ids.traverse(id =>
        EntityStore
          .standard()
          .load[AccessSessionEntity](id)
          .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
      )
    yield
      sessions

  private def _is_active_access_session(session: AccessSessionEntity): Boolean =
    session.revokedAt.isEmpty && !_is_expired(session.expiresAt)

  def isActiveRefreshSession(session: RefreshSessionEntity): Boolean =
    session.revokedAt.isEmpty && session.rotatedAt.isEmpty && !_is_expired(session.expiresAt)

  private def _is_reused_refresh_session(session: RefreshSessionEntity): Boolean =
    !_is_expired(session.expiresAt) && session.rotatedAt.nonEmpty

  private def _is_expired(expiresAt: String): Boolean =
    scala.util.Try(Instant.parse(expiresAt)).toOption.exists(_.isBefore(Instant.now))

  private def _token_hash(token: String): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = digest.digest(token.getBytes(StandardCharsets.UTF_8))
    bytes.map(b => f"${b & 0xff}%02x").mkString

  object UserAccountAuthenticationProvider extends AuthenticationProvider {
    val name: String = "textus-user-account"

    def authenticate(request: AuthenticationRequest)(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
      request.accessToken match
        case Some(token) =>
          authenticateAccessToken(token).map(Some(_))
        case None =>
          request.refreshToken match
            case Some(token) => authenticateRefreshToken(token).map(Some(_))
            case None => Consequence.success(None)
  }

  def authenticateAccessToken(
    accessToken: String
  )(using ctx: ExecutionContext): Consequence[AuthenticationResult] =
    for
      session <- _active_access_session_by_token(accessToken)
      user <- _load_user_account(session.userAccountId)
    yield
      _authentication_result(user, session, accessToken)

  def authenticateRefreshToken(
    refreshToken: String
  )(using ctx: ExecutionContext): Consequence[AuthenticationResult] =
    for
      session <- _active_refresh_session_by_token(refreshToken)
      user <- _load_user_account(session.userAccountId)
    yield
      _authentication_result(user, session, refreshToken)

  private def _load_user_account(
    userId: EntityId
  )(using ctx: ExecutionContext): Consequence[UserAccountEntity] =
    EntityStore.standard().load[UserAccountEntity](userId).flatMap(x => Consequence.successOrEntityNotFound(x)(userId))

  private def _active_refresh_session_by_token(
    token: String
  )(using ctx: ExecutionContext): Consequence[RefreshSessionEntity] =
    val hashed = _token_hash(token)
    _raw_refresh_sessions_by_token_hash(hashed).flatMap { sessions =>
      sessions.find(isActiveRefreshSession) match
        case Some(session) => Consequence.success(session)
        case None => Consequence.failure("No active refresh session matches the current refresh token.")
    }

  private def _raw_refresh_sessions_by_token_hash(
    hashedToken: String
  )(using ctx: ExecutionContext): Consequence[Vector[RefreshSessionEntity]] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      ids <- result.records.toVector
        .filter(_.getString("token_hash").contains(hashedToken))
        .traverse(record =>
          record
            .getAsC[EntityId]("id")
            .flatMap(x => Consequence.successOrPropertyNotFound("id", x))
        )
      sessions <- ids.traverse(id =>
        EntityStore
          .standard()
          .load[RefreshSessionEntity](id)
          .flatMap(x => Consequence.successOrEntityNotFound(x)(id))
      )
    yield
      sessions

  private def _authentication_result(
    user: UserAccountEntity,
    session: AccessSessionEntity,
    accessToken: String
  ): AuthenticationResult =
    AuthenticationResult(
      principalId = PrincipalId(user.id.print),
      attributes = Map(
        "user_account_id" -> user.id.print,
        "role" -> "user",
        "privilege" -> "user",
        "access_token" -> accessToken
      ),
      capabilities = Set(Capability("user")),
      level = SecurityLevel("user"),
      subjectKind = SubjectKind.User,
      session = Some(
        SessionContext(
          sessionId = Some(session.id.print),
          tokenId = Some(session.id.print),
          tokenKind = Some("access"),
          authenticatedAt = _parse_instant(session.issuedAt),
          expiresAt = _parse_instant(session.expiresAt),
          refreshSessionId = session.refreshSessionId,
          attributes = _session_attributes(session.clientId, session.deviceInfo, session.ipAddress, session.userAgent)
        )
      )
    )

  private def _authentication_result(
    user: UserAccountEntity,
    session: RefreshSessionEntity,
    refreshToken: String
  ): AuthenticationResult =
    AuthenticationResult(
      principalId = PrincipalId(user.id.print),
      attributes = Map(
        "user_account_id" -> user.id.print,
        "role" -> "user",
        "privilege" -> "user",
        "refresh_token" -> refreshToken
      ),
      capabilities = Set(Capability("user")),
      level = SecurityLevel("user"),
      subjectKind = SubjectKind.User,
      session = Some(
        SessionContext(
          sessionId = Some(session.id.print),
          tokenId = Some(session.id.print),
          tokenKind = Some("refresh"),
          authenticatedAt = _parse_instant(session.issuedAt),
          expiresAt = _parse_instant(session.expiresAt),
          refreshSessionId = Some(session.id.print),
          attributes = _session_attributes(session.clientId, session.deviceInfo, session.ipAddress, session.userAgent)
        )
      )
    )

  private def _session_attributes(
    clientId: Option[String],
    deviceInfo: Option[String],
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Map[String, String] =
    Vector(
      clientId.map("client_id" -> _),
      deviceInfo.map("device_info" -> _),
      ipAddress.map("ip_address" -> _),
      userAgent.map("user_agent" -> _)
    ).flatten.toMap

  private def _parse_instant(p: String): Option[Instant] =
    scala.util.Try(Instant.parse(p)).toOption

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

    def clear(): Unit =
      users.clear()

  private[textus] def resetLoginWorkingSetForTest(): Unit =
    LoginWorkingSet.clear()

  private[textus] def addLoggedInUserForTest(userId: EntityId)(using ExecutionContext): Consequence[Unit] =
    EntityStore.standard().load[UserAccountEntity](userId).flatMap {
      case Some(user) =>
        LoginWorkingSet.upsert(user)
        Consequence.unit
      case None =>
        Consequence.failure(s"User account not found for working set: ${userId.print}")
    }

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
