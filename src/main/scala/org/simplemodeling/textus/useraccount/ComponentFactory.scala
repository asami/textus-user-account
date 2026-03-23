package org.simplemodeling.textus.useraccount

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import cats.syntax.all.*
import org.goldenport.Consequence
import org.goldenport.datatype.Name
import org.simplemodeling.model.datatype.EntityId
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.record.Record
import org.goldenport.cncf.action.ActionCall
import org.goldenport.cncf.component.{Component, ComponentCreate}
import org.goldenport.cncf.directive.Query
import org.goldenport.cncf.unitofwork.ExecUowM
import org.simplemodeling.model.directive.{Condition, Update}

import org.simplemodeling.textus.useraccount.entity.{Credential => CredentialEntity, UserAccount => UserAccountEntity}
import org.simplemodeling.textus.useraccount.entity.create.{Credential => CredentialCreate, UserAccount => UserAccountCreate}
import org.simplemodeling.textus.useraccount.entity.query.{Credential => CredentialQuery, UserAccount => UserAccountQuery}
import org.simplemodeling.textus.useraccount.entity.update.{Credential => CredentialUpdate, UserAccount => UserAccountUpdate}

/*
 * @since   Mar. 23, 2026
 * @version Mar. 23, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentFactory extends UserAccountComponent.Factory:
  import UserAccountComponent.ManagementService
  import UserAccountComponent.UserService

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
        status <- forcedStatus match
          case Some(s) => exec_pure(s)
          case None => exec_from(requiredString(record, List("status")))
        patchRecord = Record.dataAuto("status" -> status)
        patch <- exec_from(UserAccountUpdate.createC(patchRecord))
        _ <- entity_update(userId, patch)
      yield
        OperationResponse.void

    protected final def deleteUserAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        credentials <- credentialsForUser(userId)
        _ <- deleteCredentials(credentials)
        _ <- entity_delete(userId)
      yield
        OperationResponse.void

    protected final def listUserAccounts(record: Record): ExecUowM[OperationResponse] =
      for
        status <- exec_from(optionalString(record, List("status")))
        email <- exec_from(optionalString(record, List("email")))
        query = UserAccountQuery(
          id = Condition.any[EntityId],
          name = Condition.any[Name],
          title = Condition.any[String],
          email = email.map(Condition.is[String]).getOrElse(Condition.any[String]),
          status = status.map(Condition.is[String]).getOrElse(Condition.any[String])
        )
        result <- entity_search[UserAccountEntity](UserAccountQuery.collectionId, Query(query))
      yield
        OperationResponse.create(result)

    protected final def login(record: Record): ExecUowM[OperationResponse] =
      for
        email <- exec_from(requiredString(record, List("email")))
        password <- exec_from(requiredString(record, List("password")))
        user <- userByEmail(email)
        credential <- credentialByUserAndPassword(user.id, password)
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
        _ <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
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
        userId <- exec_from(requiredEntityId(record, List("userAccountId", "user_account_id", "id")))
        user <- entity_load[UserAccountEntity](userId)
      yield
        OperationResponse(user.toRecord())

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
        status = Condition.any[String]
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

    private def requiredString(record: Record, keys: List[String]): Consequence[String] =
      recordGetAsC[String](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def requiredEntityId(record: Record, keys: List[String]): Consequence[EntityId] =
      recordGetAsC[EntityId](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def optionalString(record: Record, keys: List[String]): Consequence[Option[String]] =
      recordGetAsC[String](record, keys)

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
      updateUserStatus(action.record, Some("formal"))

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
      createUserAndCredential(action.record, Some("active"))

  private final case class ManagementUpdateUserStatusActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.UpdateUserStatusCommand
  ) extends ManagementService.UpdateUserStatusActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      updateUserStatus(action.record, None)

  private final case class ManagementDeleteUserAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.DeleteUserAccountCommand
  ) extends ManagementService.DeleteUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      deleteUserAccount(action.record)

  private final case class ManagementListUserAccountsActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.ListUserAccountsQuery
  ) extends ManagementService.ListUserAccountsActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      listUserAccounts(action.record)

object ComponentFactory:
  def create(componentCreate: ComponentCreate): Vector[Component] =
    new ComponentFactory().create(componentCreate)

  def createStandalone(): Component =
    UserAccountComponent()
