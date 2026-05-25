package org.simplemodeling.textus.useraccount

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.util.Random

import cats.syntax.all.*
import org.goldenport.Consequence
import org.goldenport.datatype.Name
import org.simplemodeling.model.datatype.{EntityCollectionId, EntityId}
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.protocol.operation.OperationResponse.RecordResponse
import org.goldenport.record.Record
import org.goldenport.protocol.{Property, Request}
import org.goldenport.cncf.action.ActionCall
import org.goldenport.cncf.component.{Component, ComponentCreate, EntityRuntimePlanProvider}
import org.goldenport.cncf.CncfVersion
import org.goldenport.cncf.context.{Capability, ExecutionContext, Principal, PrincipalId, SecurityContext, SecurityLevel, SessionContext, SubjectKind}
import org.goldenport.cncf.datastore.{Query as DsQuery, QueryDirective as DsQueryDirective}
import org.goldenport.cncf.directive.Query
import org.goldenport.cncf.entity.{EntityQuery, EntityStore}
import org.goldenport.cncf.entity.runtime.{EntityMemoryPolicy, EntityRuntimePlan, PartitionStrategy, WorkingSetDefinition}
import org.goldenport.cncf.operation.CmlOperationAccess
import org.goldenport.cncf.messagedelivery.{DeliveryChannel, UnifiedMessage, MessageDeliveryProviderRuntime}
import org.goldenport.cncf.security.{AuthenticationProvider, AuthenticationRequest, AuthenticationResult, OperationAccessPolicy}
import org.goldenport.cncf.unitofwork.{ExecUowM, UnitOfWorkAuthorization}
import org.simplemodeling.model.directive.{Condition, Update}
import org.simplemodeling.model.statemachine.ActivationStatus
import org.simplemodeling.model.value.{ResourceAttributes, ResourceAttributesUpdate}

import org.simplemodeling.textus.useraccount.entity.{AccessSession => AccessSessionEntity, Credential => CredentialEntity, RefreshSession => RefreshSessionEntity, UserAccount => UserAccountEntity, UserProfile => UserProfileEntity}
import org.simplemodeling.textus.useraccount.entity.create.{AccessSession => AccessSessionCreate, Credential => CredentialCreate, RefreshSession => RefreshSessionCreate, UserAccount => UserAccountCreate}
import org.simplemodeling.textus.useraccount.entity.query.{AccessSession => AccessSessionQuery, Credential => CredentialQuery, RefreshSession => RefreshSessionQuery, UserAccount => UserAccountQuery, UserProfile => UserProfileQuery}
import org.simplemodeling.textus.useraccount.entity.update.{AccessSession => AccessSessionUpdate, Credential => CredentialUpdate, RefreshSession => RefreshSessionUpdate, UserAccount => UserAccountUpdate}

/*
 * @since   Mar. 23, 2026
 *  version Mar. 24, 2026
 *  version Apr. 25, 2026
 *  version May.  9, 2026
 * @version May. 26, 2026
 * @author  ASAMI, Tomoharu
 */
class ComponentFactory extends UserAccountComponent.Factory with EntityRuntimePlanProvider:
  import UserAccountComponent.AggregateService
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

  override val aggregate: UserAccountComponent.AggregateServiceFactory =
    new UserAccountAggregateServiceFactory

  private final class UserAccountAggregateServiceFactory extends UserAccountComponent.AggregateServiceFactory:
    import AggregateService.*

    override def createLoadUserAccountActionCall(
      core: ActionCall.Core,
      action: LoadUserAccountQuery
    ): LoadUserAccountActionCall =
      LoadUserAccountAggregateActionCallImpl(core, action)

  private final case class LoadUserAccountAggregateActionCallImpl(
    core: ActionCall.Core,
    override val action: AggregateService.LoadUserAccountQuery
  ) extends AggregateService.LoadUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      val internalCtx = ExecutionContext.withAggregateInternalRead(executionContext, true)
      val memberQuery = Query(Record.data("userAccountId" -> action.id.value))
      for
        account <- exec_from(
          EntityStore
            .standard()
            .load[UserAccountEntity](action.id)(using summon, internalCtx)
            .flatMap(x => Consequence.successOrEntityNotFound(x)(action.id))
        )
        profiles <- exec_from(
          EntityStore
            .standard()
            .search[UserProfileEntity](EntityQuery(UserProfileEntity.collectionId, memberQuery))(using summon, internalCtx)
        )
        credentials <- exec_from(
          EntityStore
            .standard()
            .search[CredentialEntity](EntityQuery(CredentialEntity.collectionId, memberQuery))(using summon, internalCtx)
        )
        accessSessions <- exec_from(
          EntityStore
            .standard()
            .search[AccessSessionEntity](EntityQuery(AccessSessionEntity.collectionId, memberQuery))(using summon, internalCtx)
        )
        refreshSessions <- exec_from(
          EntityStore
            .standard()
            .search[RefreshSessionEntity](EntityQuery(RefreshSessionEntity.collectionId, memberQuery))(using summon, internalCtx)
        )
      yield
        OperationResponse.RecordResponse(
          account.toRecord() ++ Record.dataAuto(
            "userProfile" -> profiles.data.map(_.toRecord()),
            "credential" -> credentials.data.map(_.toRecord()),
            "accessSession" -> accessSessions.data.map(_.toRecord()),
            "refreshSession" -> refreshSessions.data.map(_.toRecord())
          )
        )

  override protected def create_Component(params: ComponentCreate): Component =
    new UserAccountComponent() {
      override def authenticationProviders: Vector[AuthenticationProvider] =
        Vector(ComponentFactory.userAccountAuthenticationProvider(this))
    }

  override val view = new UserAccountComponent.ViewServiceFactory() {
    override def createLoadUserProfileActionCall(
      core: ActionCall.Core,
      action: UserAccountComponent.ViewService.LoadUserProfileQuery
    ): UserAccountComponent.ViewService.LoadUserProfileActionCall =
      LoadUserProfileViewActionCallImpl(core, action)
  }

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
      ),
      EntityRuntimePlan(
        entityName = UserProfileQuery.collectionId.name,
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
      action: UserService.UserProvisionalRegistration
    ): UserService.ProvisionalRegistrationActionCall =
      ProvisionalRegistrationActionCallImpl(core, action)

    override def createRegisterActionCall(
      core: ActionCall.Core,
      action: UserService.UserRegister
    ): UserService.RegisterActionCall =
      RegisterActionCallImpl(core, action)

    override def createPromoteToFormalRegistrationActionCall(
      core: ActionCall.Core,
      action: UserService.UserPromoteToFormalRegistration
    ): UserService.PromoteToFormalRegistrationActionCall =
      PromoteToFormalRegistrationActionCallImpl(core, action)

    override def createLoginActionCall(
      core: ActionCall.Core,
      action: UserService.UserLogin
    ): UserService.LoginActionCall =
      LoginActionCallImpl(core, action)

    override def createLogoutActionCall(
      core: ActionCall.Core,
      action: UserService.UserLogout
    ): UserService.LogoutActionCall =
      LogoutActionCallImpl(core, action)

    override def createLogoutAllActionCall(
      core: ActionCall.Core,
      action: UserService.UserLogoutAll
    ): UserService.LogoutAllActionCall =
      LogoutAllActionCallImpl(core, action)

    override def createRefreshAccessTokenActionCall(
      core: ActionCall.Core,
      action: UserService.UserRefreshAccessToken
    ): UserService.RefreshAccessTokenActionCall =
      RefreshAccessTokenActionCallImpl(core, action)

    override def createChangePasswordActionCall(
      core: ActionCall.Core,
      action: UserService.UserChangePassword
    ): UserService.ChangePasswordActionCall =
      ChangePasswordActionCallImpl(core, action)

    override def createVerifyMyEmailActionCall(
      core: ActionCall.Core,
      action: UserService.UserVerifyMyEmail
    ): UserService.VerifyMyEmailActionCall =
      VerifyMyEmailActionCallImpl(core, action)

    override def createVerifyMyPhoneActionCall(
      core: ActionCall.Core,
      action: UserService.UserVerifyMyPhone
    ): UserService.VerifyMyPhoneActionCall =
      VerifyMyPhoneActionCallImpl(core, action)

    override def createRequestPasswordResetActionCall(
      core: ActionCall.Core,
      action: UserService.UserRequestPasswordReset
    ): UserService.RequestPasswordResetActionCall =
      RequestPasswordResetActionCallImpl(core, action)

    override def createConfirmPasswordResetActionCall(
      core: ActionCall.Core,
      action: UserService.UserConfirmPasswordReset
    ): UserService.ConfirmPasswordResetActionCall =
      ConfirmPasswordResetActionCallImpl(core, action)

    override def createEnrollTwoFactorActionCall(
      core: ActionCall.Core,
      action: UserService.UserEnrollTwoFactor
    ): UserService.EnrollTwoFactorActionCall =
      EnrollTwoFactorActionCallImpl(core, action)

    override def createVerifyTwoFactorLoginActionCall(
      core: ActionCall.Core,
      action: UserService.UserVerifyTwoFactorLogin
    ): UserService.VerifyTwoFactorLoginActionCall =
      VerifyTwoFactorLoginActionCallImpl(core, action)

    override def createGetMyAccountActionCall(
      core: ActionCall.Core,
      action: UserService.UserGetMyAccount
    ): UserService.GetMyAccountActionCall =
      GetMyAccountActionCallImpl(core, action)

    override def createLookupUserByLoginNameActionCall(
      core: ActionCall.Core,
      action: UserService.UserLookupByLoginName
    ): UserService.LookupUserByLoginNameActionCall =
      LookupUserByLoginNameActionCallImpl(core, action)

  override val Management: UserAccountComponent.ManagementServiceFactory = new UserAccountComponent.ManagementServiceFactory:
    override def createCreateUserAccountActionCall(
      core: ActionCall.Core,
      action: ManagementService.AdminCreateUserAccount
    ): ManagementService.CreateUserAccountActionCall =
      ManagementCreateUserAccountActionCallImpl(core, action)

    override def createUpdateUserStatusActionCall(
      core: ActionCall.Core,
      action: ManagementService.AdminUpdateUserStatus
    ): ManagementService.UpdateUserStatusActionCall =
      ManagementUpdateUserStatusActionCallImpl(core, action)

    override def createDeleteUserAccountActionCall(
      core: ActionCall.Core,
      action: ManagementService.AdminDeleteUserAccount
    ): ManagementService.DeleteUserAccountActionCall =
      ManagementDeleteUserAccountActionCallImpl(core, action)

    override def createListUserAccountsActionCall(
      core: ActionCall.Core,
      action: ManagementService.AdminListUserAccounts
    ): ManagementService.ListUserAccountsActionCall =
      ManagementListUserAccountsActionCallImpl(core, action)

  private trait UserAccountActionSupport:
    self: org.goldenport.cncf.action.ActionCall =>

    protected final def createUserAndCredential(
      record: Record,
      defaultStatus: Option[String]
    ): ExecUowM[OperationResponse] =
      val normalizedrecord = ComponentFactory.normalizeDataStoreRecord(record)
      for
        password <- exec_from(requiredString(normalizedrecord, List("password")))
        userRecord0 <- exec_from(_with_registration_email(normalizedrecord))
        userRecord = _with_default_status(userRecord0, defaultStatus)
        email <- exec_from(requiredString(userRecord, List("email")))
        _ <- _require_email_available(email)
        loginName <- exec_from(requiredString(userRecord, List("loginName")))
        _ <- _require_login_name_available(loginName)
        status <- exec_from(_required_status(userRecord))
        _ <- exec_from(ComponentFactory.requireCreatableStatus(status))
        user0 <- exec_from(UserAccountCreate.createWithExecutionContextC(userRecord)(using executionContext))
        user = user0.withResourceAttributes(ResourceAttributes(activationStatus = ComponentFactory.activationStatusForUserAccountStatus(status)))
        created <- entity_create(user)
        credentialRecord = Record.dataAuto(
          "userAccountId" -> created.id,
          "passwordHash" -> _password_hash(password)
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
        userId <- exec_from(_required_entity_id(record, List("userAccountId", "id")))
        user <- entity_load[UserAccountEntity](userId)
        targetStatus <- forcedStatus match
          case Some(s) => exec_from(ComponentFactory.parseStatus(s))
          case None => exec_from(_required_status(record))
        suspensionReason <- exec_from(_optional_string(record, List("suspensionReason")))
        _ <- exec_from(ComponentFactory.requireTransition(user.status, targetStatus))
        _ <- _update_user_account_fields(
          userId,
          ComponentFactory.statusUpdateFields(targetStatus, suspensionReason)(using executionContext)
        )
      yield
        OperationResponse.void

    protected final def deleteUserAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_required_entity_id(record, List("userAccountId", "id")))
        credentials <- _credentials_for_user(userId)
        _ <- _delete_credentials(credentials)
        _ <- exec_from(_remove_logged_in_user(userId))
        _ <- entity_delete(userId)
      yield
        OperationResponse.void

    protected final def listUserAccounts(record: Record): ExecUowM[OperationResponse] =
      for
        status <- exec_from(_optional_status(record))
        email <- exec_from(_optional_string(record, List("email")))
        loginName <- exec_from(_optional_string(record, List("loginName")))
        offset <- exec_from(_optional_int(record, List("offset")))
        limit <- exec_from(_optional_int(record, List("limit")))
        condition = UserAccountQuery(
          id = Condition.any[EntityId],
          name = Condition.any[Name],
          title = Condition.any[String],
          email = email.map(Condition.is[String]).getOrElse(Condition.any[String]),
          loginName = loginName.map(Condition.is[String]).getOrElse(Condition.any[String]),
          externalSubjectId = Condition.any[String],
          emailVerifiedAt = Condition.any[Instant],
          phoneNumber = Condition.any[String],
          locale = Condition.any[String],
          timeZone = Condition.any[String],
          phoneVerifiedAt = Condition.any[Instant],
          lastLoginAt = Condition.any[Instant],
          passwordChangedAt = Condition.any[Instant],
          suspendedAt = Condition.any[Instant],
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
        identifier <- exec_from(requiredString(record, List("identifier", "username", "email", "loginName", "handle")))
        password <- exec_from(requiredString(record, List("password")))
        user <- _user_by_login_identifier(identifier)
        _ <- exec_from(ComponentFactory.requireAuthenticatable(user.status))
        credential <- _credential_by_user_and_password(user.id, password)
        response <-
          if ComponentFactory.isTwoFactorEnrolled(user.id) then
            for
              challenge <- exec_from(ComponentFactory._issue_two_factor_login_challenge(user.id, user.email, user.loginName.getOrElse(user.email)))
              _ <- exec_from(_send_email_notification(
                recipient = user.email,
                subject = "Your Textus verification code",
                body = s"Use verification code ${challenge.code}. Challenge ID: ${challenge.challengeId}.",
                templateId = Some("two-factor-login"),
                attributes = Map(
                  "challengeId" -> challenge.challengeId,
                  "code" -> challenge.code,
                  "handle" -> user.loginName.getOrElse(user.email)
                )
              ))
            yield
              OperationResponse(
                Record.data(
                  "userAccountId" -> user.id.print,
                  "credentialId" -> credential.id.print,
                  "twoFactorRequired" -> true,
                  "challengeId" -> challenge.challengeId,
                  "channel" -> "email",
                  "authenticated" -> false
                )
              )
          else
            _issue_login_response(user, credential.id)
      yield
        response

    protected final def logout(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_required_entity_id(record, List("userAccountId", "id")))
        _ <- _require_user_session(userId)
        accessSession <- _require_current_access_session(userId)
        _ <- _revoke_access_session(accessSession.id)
        _ <- _revoke_linked_refresh_session(accessSession)
        _ <- exec_from(_remove_logged_in_user(userId))
      yield
        OperationResponse.void

    protected final def logoutAll(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_required_entity_id(record, List("userAccountId", "id")))
        _ <- _revoke_all_access_sessions(userId)
        _ <- _revoke_all_refresh_sessions(userId)
        _ <- exec_from(_remove_logged_in_user(userId))
      yield
        OperationResponse.void

    protected final def refreshAccessToken(record: Record): ExecUowM[OperationResponse] =
      for
        refreshToken <- exec_from(requiredString(record, List("refreshToken")))
        refreshSession <- _active_refresh_session_by_token(refreshToken)
        _ <- _revoke_refresh_session_as_rotated(refreshSession.id)
        refreshIssued <- _issue_refresh_session(
          refreshSession.userAccountId,
          None,
          _client_id_from_security_context().orElse(refreshSession.clientId),
          _device_info_from_security_context().orElse(refreshSession.deviceInfo),
          _ip_address_from_security_context().orElse(refreshSession.ipAddress),
          _user_agent_from_security_context().orElse(refreshSession.userAgent),
          predecessor = Some(refreshSession.id)
        )
        accessIssued <- _issue_access_session(
          refreshSession.userAccountId,
          None,
          Some(refreshIssued.sessionId),
          _client_id_from_security_context().orElse(refreshSession.clientId),
          _device_info_from_security_context().orElse(refreshSession.deviceInfo),
          _ip_address_from_security_context().orElse(refreshSession.ipAddress),
          _user_agent_from_security_context().orElse(refreshSession.userAgent)
        )
        _ <- exec_from(_add_logged_in_user_for_user_id(refreshSession.userAccountId))
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
        userId <- exec_from(_required_entity_id(record, List("userAccountId", "id")))
        current <- exec_from(requiredString(record, List("currentPassword")))
        next <- exec_from(requiredString(record, List("newPassword")))
        credential <- _credential_by_user_and_password(userId, current)
        patch <- exec_from(CredentialUpdate.createC(Record.dataAuto("passwordHash" -> _password_hash(next))))
        _ <- entity_update(credential.id, patch)
        _ <- _update_password_changed_at(userId)
        _ <- _revoke_all_access_sessions(userId)
        _ <- _revoke_all_refresh_sessions(userId)
      yield
        OperationResponse.void

    protected final def getMyAccount(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_current_user_id(record))
        user <- entity_load[UserAccountEntity](userId)
      yield
        OperationResponse(_public_user_record(user))

    protected final def verifyMyEmail(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_current_user_id(record))
        user <- _raw_user_account_record(userId)
        proofToken <- exec_from(requiredString(record, List("proofToken")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        _ <- exec_from(ComponentFactory.requireEmailVerificationPending(user))
        _ <- _update_email_verified_at(userId)
      yield
        OperationResponse.void

    protected final def verifyMyPhone(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_current_user_id(record))
        user <- _raw_user_account_record(userId)
        phoneNumber <- exec_from(requiredString(record, List("phoneNumber")))
        proofToken <- exec_from(requiredString(record, List("proofToken")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        _ <- exec_from(ComponentFactory.requirePhoneVerificationPending(user, phoneNumber))
        _ <- _update_phone_verification(userId, phoneNumber)
      yield
        OperationResponse.void

    protected final def requestPasswordReset(record: Record): ExecUowM[OperationResponse] =
      for
        email <- exec_from(requiredString(record, List("email")))
        _users <- _raw_user_accounts_by_email(email)
        _ <- _users.headOption match
          case Some(user) =>
            for
              _ <- exec_from(ComponentFactory.requireAuthenticatable(user.status))
              token <- exec_from(ComponentFactory._issue_password_reset_token(user.id, user.email, user.loginName.getOrElse(user.email)))
              _ <- exec_from(_send_email_notification(
                recipient = user.email,
                subject = "Reset your Textus password",
                body = s"Open /web/textus-user-account/password-reset/confirm?token=${token.token} to continue.",
                templateId = Some("password-reset"),
                attributes = Map(
                  "resetToken" -> token.token,
                  "handle" -> user.loginName.getOrElse(user.email)
                )
              ))
            yield
              ()
          case None => exec_from(Consequence.unit)
      yield
        OperationResponse(Record.data("accepted" -> true))

    protected final def confirmPasswordReset(record: Record): ExecUowM[OperationResponse] =
      for
        token <- exec_from(requiredString(record, List("token", "resetToken")))
        next <- exec_from(requiredString(record, List("newPassword", "password")))
        reset <- exec_from(ComponentFactory._consume_password_reset_token(token))
        credentials <- _raw_credentials_by_user(reset.userId)
        credential <- exec_from(_first_or_failure(credentials, s"credential not found for user: ${reset.userId.print}"))
        _ <- _update_credential_fields_direct(
          credential.id,
          Record.dataAuto("passwordHash" -> _password_hash(next))
        )
        _ <- _update_password_changed_at(reset.userId)
        _ <- _revoke_all_access_sessions(reset.userId)
        _ <- _revoke_all_refresh_sessions(reset.userId)
      yield
        OperationResponse(Record.data("reset" -> true))

    protected final def _enroll_two_factor(record: Record): ExecUowM[OperationResponse] =
      for
        userId <- exec_from(_current_user_id(record))
        userRecord <- _raw_user_account_record(userId)
        user <- exec_from(UserAccountEntity.createC(userRecord))
        _ <- exec_from(ComponentFactory._enroll_two_factor(user.id, user.email, user.loginName.getOrElse(user.email)))
        _ <- exec_from(_send_email_notification(
          recipient = user.email,
          subject = "Two-factor authentication enabled",
          body = s"Email verification codes are now required for ${user.loginName.getOrElse(user.email)}.",
          templateId = Some("two-factor-enrolled"),
          attributes = Map("handle" -> user.loginName.getOrElse(user.email))
        ))
      yield
        OperationResponse(Record.data("enrolled" -> true, "channel" -> "email"))

    protected final def verifyTwoFactorLogin(record: Record): ExecUowM[OperationResponse] =
      for
        challengeId <- exec_from(requiredString(record, List("challengeId")))
        code <- exec_from(requiredString(record, List("code", "verificationCode")))
        challenge <- exec_from(ComponentFactory._verify_two_factor_login_challenge(challengeId, code))
        userRecord <- _raw_user_account_record(challenge.userId)
        user <- exec_from(UserAccountEntity.createC(userRecord))
        credentials <- _raw_credentials_by_user(user.id)
        credential <- exec_from(_first_or_failure(credentials, s"credential not found for user: ${user.id.print}"))
        response <- _issue_login_response(user, credential.id)
      yield
        response

    protected final def requireManagementPrivilege(): Consequence[Unit] =
      ComponentFactory.requireManagementPrivilege(using executionContext)

    private def _with_default_status(record: Record, defaultstatus: Option[String]): Record =
      defaultstatus match
        case Some(status) if !_has_string_value(record, List("status")) =>
          record ++ Record.data("status" -> status)
        case _ =>
          record

    private def _with_registration_email(record: Record): Consequence[Record] =
      if _has_string_value(record, List("email")) then
        Consequence.success(record)
      else
        requiredString(record, List("identifier")).map { identifier =>
          record ++ Record.data("email" -> identifier)
        }

    private def _user_by_email(email: String): ExecUowM[UserAccountEntity] =
      for
        _users <- _raw_user_accounts_by_email(email)
        user <- exec_from(_first_or_failure(_users, s"user account not found by email: $email"))
      yield
        user

    private def _user_by_login_identifier(identifier: String): ExecUowM[UserAccountEntity] =
      if identifier.contains("@") then _user_by_email(identifier) else _user_by_login_name(identifier)

    private def _require_email_available(email: String): ExecUowM[Unit] =
      for
        _users <- _raw_user_accounts_by_email(email)
        _ <- exec_from(ComponentFactory.requireEmailAvailable(email, _users))
      yield
        ()

    private def _raw_user_accounts_by_login_name(loginname: String): ExecUowM[Vector[UserAccountEntity]] =
      for
        records <- _raw_records(UserAccountQuery.collectionId)
        _users <- exec_from(
          records
            .filter(_.getString("loginName").contains(loginname))
            .traverse(UserAccountEntity.createC)
        )
      yield
        _users

    private def _user_by_login_name(loginname: String): ExecUowM[UserAccountEntity] =
      for
        _users <- _raw_user_accounts_by_login_name(loginname)
        user <- exec_from(_first_or_failure(_users, s"user account not found by loginName: $loginname"))
      yield
        user

    private def _require_login_name_available(loginname: String): ExecUowM[Unit] =
      for
        _users <- _raw_user_accounts_by_login_name(loginname)
        _ <- exec_from(ComponentFactory.requireLoginNameAvailable(loginname, _users))
      yield
        ()

    private def _send_email_notification(
      recipient: String,
      subject: String,
      body: String,
      templateId: Option[String] = None,
      attributes: Map[String, String] = Map.empty
    ): Consequence[Unit] =
      val base = component.map(_.logic.executionContext()).getOrElse(executionContext)
      MessageDeliveryProviderRuntime.send(
        base,
        UnifiedMessage(
          channel = DeliveryChannel.Email,
          recipient = recipient,
          subject = Some(subject),
          body = body,
          templateId = templateId,
          attributes = attributes
        )
      ).map(_ => ())

    private def _issue_login_response(
      user: UserAccountEntity,
      credentialId: EntityId
    ): ExecUowM[OperationResponse] =
      for
        refreshIssued <- _issue_refresh_session(
          user.id,
          None,
          _client_id_from_security_context(),
          _device_info_from_security_context(),
          _ip_address_from_security_context(),
          _user_agent_from_security_context()
        )
        issued <- _issue_access_session(
          user.id,
          None,
          Some(refreshIssued.sessionId),
          _client_id_from_security_context(),
          _device_info_from_security_context(),
          _ip_address_from_security_context(),
          _user_agent_from_security_context()
        )
        _ <- _update_last_login_at(user.id)
        _ <- exec_from(_add_logged_in_user(user))
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "credentialId" -> credentialId.print,
            "accessSessionId" -> issued.sessionId.print,
            "accessToken" -> issued.rawToken,
            "refreshSessionId" -> refreshIssued.sessionId.print,
            "refreshToken" -> refreshIssued.rawToken,
            "authenticated" -> true
          )
        )

    private def _public_user_record(user: UserAccountEntity): Record =
      ComponentFactory.normalizeDataStoreRecord(user.toRecord()) ++ Record.data("handle" -> user.loginName.orNull)

    protected final def lookupUserByLoginName(record: Record): ExecUowM[OperationResponse] =
      for
        loginName <- exec_from(requiredString(record, List("loginName")))
        user <- _user_by_login_name(loginName)
        _ <- exec_from(ComponentFactory.requireAuthenticatable(user.status))
      yield
        OperationResponse(_public_user_record(user))

    private def _credentials_for_user(userid: EntityId): ExecUowM[Vector[CredentialEntity]] =
      val query = CredentialQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userid),
        passwordHash = Condition.any[String]
      )
      entity_search[CredentialEntity](CredentialQuery.collectionId, Query(query)).map(_.data)

    private def _access_sessions_for_user(userid: EntityId): ExecUowM[Vector[AccessSessionEntity]] =
      val query = AccessSessionQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userid),
        refreshSessionId = Condition.any[String],
        tokenHash = Condition.any[String],
        issuedAt = Condition.any[Instant],
        expiresAt = Condition.any[Instant],
        revokedAt = Condition.any[Instant],
        lastAccessedAt = Condition.any[Instant],
        clientId = Condition.any[String],
        deviceInfo = Condition.any[String],
        ipAddress = Condition.any[String],
        userAgent = Condition.any[String]
      )
      entity_search[AccessSessionEntity](AccessSessionQuery.collectionId, Query(query)).map(_.data)

    private def _refresh_sessions_for_user(userid: EntityId): ExecUowM[Vector[RefreshSessionEntity]] =
      val query = RefreshSessionQuery(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        userAccountId = Condition.is(userid),
        successorSessionId = Condition.any[String],
        tokenHash = Condition.any[String],
        issuedAt = Condition.any[Instant],
        expiresAt = Condition.any[Instant],
        revokedAt = Condition.any[Instant],
        rotatedAt = Condition.any[Instant],
        clientId = Condition.any[String],
        deviceInfo = Condition.any[String],
        ipAddress = Condition.any[String],
        userAgent = Condition.any[String]
      )
      entity_search[RefreshSessionEntity](RefreshSessionQuery.collectionId, Query(query)).map(_.data)

    private def _credential_by_user_and_password(
      userId: EntityId,
      password: String
    ): ExecUowM[CredentialEntity] =
      for
        credentials <- _raw_credentials_by_user_and_password(userId, _password_hash(password))
        credential <- exec_from(_first_or_failure(credentials, "invalid credentials"))
      yield
        credential

    private def _raw_user_accounts_by_email(email: String): ExecUowM[Vector[UserAccountEntity]] =
      for
        records <- _raw_records(UserAccountQuery.collectionId)
        _users <- exec_from(
          records
            .filter(_.getString("email").contains(email))
            .traverse(UserAccountEntity.createC)
        )
      yield
        _users

    private def _raw_credentials_by_user(
      userId: EntityId
    ): ExecUowM[Vector[CredentialEntity]] =
      for
        records <- _raw_records(CredentialQuery.collectionId)
        credentials <- exec_from(
          records
            .filter(_.getString("userAccountId").contains(userId.print))
            .traverse(CredentialEntity.createC)
        )
      yield
        credentials

    private def _raw_credentials_by_user_and_password(
      userId: EntityId,
      hashedPassword: String
    ): ExecUowM[Vector[CredentialEntity]] =
      for
        records <- _raw_records(CredentialQuery.collectionId)
        credentials <- exec_from(
          records
            .filter(r =>
              r.getString("userAccountId").contains(userId.print) &&
                r.getString("passwordHash").contains(hashedPassword)
            )
            .traverse(CredentialEntity.createC)
        )
      yield
        credentials

    private def _raw_access_sessions_by_token_hash(
      hashedToken: String
    ): ExecUowM[Vector[AccessSessionEntity]] =
      for
        records <- _raw_records(AccessSessionQuery.collectionId)
        sessions <- exec_from(
          records
            .filter(_.getString("tokenHash").contains(hashedToken))
            .traverse(AccessSessionEntity.createC)
        )
      yield
        sessions

    private def _raw_refresh_sessions_by_token_hash(
      hashedToken: String
    ): ExecUowM[Vector[RefreshSessionEntity]] =
      for
        records <- _raw_records(RefreshSessionQuery.collectionId)
        sessions <- exec_from(
          records
            .filter(_.getString("tokenHash").contains(hashedToken))
            .traverse(RefreshSessionEntity.createC)
        )
      yield
        sessions

    private def _raw_access_sessions_for_user(
      userId: EntityId
    ): ExecUowM[Vector[AccessSessionEntity]] =
      for
        records <- _raw_records(AccessSessionQuery.collectionId)
        sessions <- exec_from(
          records
            .filter(_.getString("userAccountId").contains(userId.print))
            .traverse(AccessSessionEntity.createC)
        )
      yield
        sessions

    private def _raw_refresh_sessions_for_user(
      userId: EntityId
    ): ExecUowM[Vector[RefreshSessionEntity]] =
      for
        records <- _raw_records(RefreshSessionQuery.collectionId)
        sessions <- exec_from(
          records
            .filter(_.getString("userAccountId").contains(userId.print))
            .traverse(RefreshSessionEntity.createC)
        )
      yield
        sessions

    private def _raw_user_account_record(userid: EntityId): ExecUowM[Record] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          recordOption <- ds.load(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(userid)
          )(using executionContext)
          record <- recordOption match
            case Some(x) => Consequence.success(x)
            case None => Consequence.entityNotFound(s"User account not found: ${userid.print}")
        yield
          _normalize_data_store_record(record)
      }

    private def _raw_records(collectionid: org.simplemodeling.model.datatype.EntityCollectionId): ExecUowM[Vector[Record]] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionid)
          result <- executionContext.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))(using executionContext)
        yield
          result.records.toVector.map(_normalize_data_store_record)
      }

    private def _normalize_data_store_record(record: Record): Record =
      ComponentFactory.normalizeDataStoreRecord(record)

    private def _entity_id_of(record: Record): Consequence[EntityId] =
      record
        .getAsC[EntityId]("id")
        .flatMap(x => Consequence.successOrPropertyNotFound("id", x))

    private def _delete_credentials(credentials: Vector[CredentialEntity]): ExecUowM[Unit] =
      credentials.foldLeft(exec_pure(())) { (z, credential) =>
        z.flatMap(_ => entity_delete(credential.id))
      }

    private def _revoke_all_access_sessions(userid: EntityId): ExecUowM[Unit] =
      for
        sessions <- _raw_access_sessions_for_user(userid)
        _ <- sessions.foldLeft(exec_pure(())) { (z, session) =>
          z.flatMap(_ => _revoke_access_session(session.id))
        }
      yield
        ()

    private def _revoke_all_refresh_sessions(userid: EntityId): ExecUowM[Unit] =
      for
        sessions <- _raw_refresh_sessions_for_user(userid)
        _ <- sessions.foldLeft(exec_pure(())) { (z, session) =>
          z.flatMap(_ => _revoke_refresh_session(session.id))
        }
      yield
        ()

    private def _issue_access_session(
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
          "tokenHash" -> _token_hash(rawToken),
          "issuedAt" -> now.toString,
          "expiresAt" -> now.plusSeconds(ComponentFactory.AccessSessionTtlSeconds.toLong).toString,
          "clientId" -> clientId,
          "deviceInfo" -> deviceInfo,
          "ipAddress" -> ipAddress,
          "userAgent" -> userAgent
        )
        session0 <- exec_from(AccessSessionCreate.createC(sessionRecord))
        session = session0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
        created <- entity_create(session)
      yield
        ComponentFactory.IssuedAccessSession(created.id, rawToken)

    private def _issue_refresh_session(
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
          "tokenHash" -> _token_hash(rawToken),
          "issuedAt" -> now.toString,
          "expiresAt" -> now.plusSeconds(ComponentFactory.RefreshSessionTtlSeconds.toLong).toString,
          "clientId" -> clientId,
          "deviceInfo" -> deviceInfo,
          "ipAddress" -> ipAddress,
          "userAgent" -> userAgent
        )
        session0 <- exec_from(RefreshSessionCreate.createC(sessionRecord))
        session = session0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
        created <- entity_create(session)
        _ <- predecessor match
          case Some(previousId) => _update_refresh_successor(previousId, created.id)
          case None => exec_pure(())
      yield
        ComponentFactory.IssuedRefreshSession(created.id, rawToken)

    private def _require_current_access_session(userid: EntityId): ExecUowM[AccessSessionEntity] =
      _current_access_token() match
        case Some(token) =>
          for
            sessions <- _raw_access_sessions_by_token_hash(_token_hash(token))
            session <- exec_from(
              sessions.find(session => session.userAccountId == userid && ComponentFactory._is_active_access_session(session)) match
                case Some(x) => Consequence.success(x)
                case None => Consequence.securityAuthenticationRequired("No current access session matches the target account.")
            )
          yield
            session
        case None =>
          exec_from(Consequence.securityAuthenticationRequired("No current access token is available for logout."))

    private def _revoke_linked_refresh_session(accesssession: AccessSessionEntity): ExecUowM[Unit] =
      accesssession.refreshSessionId match
        case Some(refreshId) =>
          for
            id <- exec_from(EntityId.parse(refreshId))
            _ <- _revoke_refresh_session(id)
          yield
            ()
        case None =>
          _current_refresh_token() match
            case Some(token) =>
              for
                sessions <- _raw_refresh_sessions_by_token_hash(_token_hash(token))
                _ <- sessions.find(_.userAccountId == accesssession.userAccountId) match
                  case Some(session) => _revoke_refresh_session(session.id)
                  case None => exec_pure(())
              yield
                ()
            case None =>
              exec_pure(())

    private def _revoke_access_session(sessionid: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now)
        _ <- _update_access_session_fields_direct(
          sessionid,
          Record.dataAuto(
            "revokedAt" -> now,
            "lastAccessedAt" -> now
          )
        )
      yield
        ()

    private def _revoke_refresh_session(sessionid: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now)
        _ <- _update_refresh_session_fields_direct(
          sessionid,
          Record.dataAuto("revokedAt" -> now)
        )
      yield
        ()

    private def _revoke_refresh_session_as_rotated(sessionid: EntityId): ExecUowM[Unit] =
      for
        now <- exec_pure(Instant.now)
        _ <- _update_refresh_session_fields_direct(
          sessionid,
          Record.dataAuto(
            "revokedAt" -> now,
            "rotatedAt" -> now
          )
        )
      yield
        ()

    private def _update_refresh_successor(previousid: EntityId, successorid: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_refresh_session_fields_direct(
          previousid,
          Record.dataAuto("successorSessionId" -> successorid.print)
        )
      yield
        ()

    private def _require_user_session(userid: EntityId): ExecUowM[Unit] =
      exec_from(ComponentFactory.requireLoggedIn(userid)(using executionContext))

    private def _active_refresh_session_by_token(refreshtoken: String): ExecUowM[RefreshSessionEntity] =
      for
        sessions <- _raw_refresh_sessions_by_token_hash(_token_hash(refreshtoken))
        session <- sessions.find(ComponentFactory.isActiveRefreshSession) match
          case Some(x) =>
            exec_pure(x)
          case None =>
            _handle_refresh_token_reuse_or_failure(sessions)
      yield
        session

    private def _handle_refresh_token_reuse_or_failure(
      sessions: Vector[RefreshSessionEntity]
    ): ExecUowM[RefreshSessionEntity] =
      if (sessions.exists(ComponentFactory._is_reused_refresh_session))
        for
          _ <- sessions.headOption match
            case Some(session) =>
              _revoke_all_access_sessions(session.userAccountId).flatMap(_ => _revoke_all_refresh_sessions(session.userAccountId))
            case None =>
              exec_pure(())
          session <- exec_from(Consequence.securityPermissionDenied[RefreshSessionEntity]("refresh token reuse detected"))
        yield
          session
      else
        exec_from(Consequence.securityPermissionDenied("invalid refresh token"))

    private def _update_last_login_at(userid: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userid,
          Record.dataAuto("lastLoginAt" -> Instant.now)
        )
      yield
        ()

    private def _update_password_changed_at(userid: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userid,
          Record.dataAuto("passwordChangedAt" -> Instant.now)
        )
      yield
        ()

    private def _update_email_verified_at(userid: EntityId): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userid,
          Record.dataAuto("emailVerifiedAt" -> Instant.now)
        )
      yield
        ()

    private def _update_phone_verification(userid: EntityId, phonenumber: String): ExecUowM[Unit] =
      for
        _ <- _update_user_account_fields_direct(
          userid,
          Record.dataAuto(
            "phoneNumber" -> phonenumber,
            "phoneVerifiedAt" -> Instant.now
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
            ComponentFactory._data_store_update_record(changes)
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
            ComponentFactory._data_store_update_record(changes)
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
            ComponentFactory._data_store_update_record(changes)
          )(using executionContext)
        yield
          ()
      }

    private def _update_credential_fields_direct(
      credentialId: EntityId,
      changes: Record
    ): ExecUowM[Unit] =
      exec_from {
        for
          cid <- executionContext.entityStoreSpace.dataStoreCollection(CredentialQuery.collectionId)
          ds <- executionContext.dataStoreSpace.dataStore(cid)
          _ <- ds.update(
            cid,
            org.goldenport.cncf.datastore.DataStore.EntryId(credentialId),
            ComponentFactory._data_store_update_record(changes)
          )(using executionContext)
        yield
          ()
      }

    private def _password_hash(password: String): String =
      _token_hash(password)

    private def _token_hash(token: String): String =
      val digest = MessageDigest.getInstance("SHA-256")
      val bytes = digest.digest(token.getBytes(StandardCharsets.UTF_8))
      bytes.map(b => f"${b & 0xff}%02x").mkString

    private def _current_access_token(): Option[String] =
      executionContext.security.principal.attributes.get("accessToken")
        .filter(_.trim.nonEmpty)

    private def _current_refresh_token(): Option[String] =
      executionContext.security.principal.attributes.get("refreshToken")
        .filter(_.trim.nonEmpty)

    private def _access_token_from_security_context(): Option[String] =
      _current_access_token()

    private def _refresh_token_from_security_context(): Option[String] =
      _current_refresh_token()

    private def _client_id_from_security_context(): Option[String] =
      executionContext.security.principal.attributes.get("clientId")
        .filter(_.trim.nonEmpty)

    private def _device_info_from_security_context(): Option[String] =
      executionContext.security.principal.attributes.get("deviceInfo")
        .filter(_.trim.nonEmpty)

    private def _ip_address_from_security_context(): Option[String] =
      executionContext.security.principal.attributes.get("ipAddress")
        .filter(_.trim.nonEmpty)

    private def _user_agent_from_security_context(): Option[String] =
      executionContext.security.principal.attributes.get("userAgent")
        .filter(_.trim.nonEmpty)

    private def _add_logged_in_user_for_user_id(userid: EntityId): Consequence[Unit] =
      ComponentFactory.addLoggedInUserForTest(userid)(using executionContext)

    private def _add_logged_in_user(user: UserAccountEntity): Consequence[Unit] =
      ComponentFactory.LoginWorkingSet.upsert(user)
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.put(user))
      Consequence.unit

    private def _remove_logged_in_user(userid: EntityId): Consequence[Unit] =
      ComponentFactory.LoginWorkingSet.remove(userid)
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.remove(userid))
      Consequence.unit

    protected final def requiredString(record: Record, keys: List[String]): Consequence[String] =
      _record_get_as_c[String](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def _required_entity_id(record: Record, keys: List[String]): Consequence[EntityId] =
      _record_get_as_c[EntityId](record, keys).flatMap(v => Consequence.successOrPropertyNotFound(keys.head, v))

    private def _optional_string(record: Record, keys: List[String]): Consequence[Option[String]] =
      _record_get_as_c[String](record, keys)

    private def _optional_int(record: Record, keys: List[String]): Consequence[Option[Int]] =
      _record_get_as_c[Int](record, keys)

    private def _required_status(record: Record): Consequence[UserAccountStatus] =
      _record_get_as_c[UserAccountStatus](record, List("status")).flatMap(v => Consequence.successOrPropertyNotFound("status", v))

    private def _optional_status(record: Record): Consequence[Option[UserAccountStatus]] =
      _record_get_as_c[UserAccountStatus](record, List("status"))

    private def _current_user_id(record: Record): Consequence[EntityId] =
      _record_get_as_c[EntityId](record, List("userAccountId", "id")).flatMap {
        case Some(id) => ComponentFactory.currentLoggedInUserId(id)(using executionContext)
        case None => ComponentFactory.currentLoggedInUserId()(using executionContext)
      }

    private def _has_string_value(record: Record, keys: List[String]): Boolean =
      _record_get_as_c[String](record, keys) match
        case Consequence.Success(Some(_)) => true
        case _ => false

    private def _first_or_failure[A](xs: Vector[A], message: String): Consequence[A] =
      xs.headOption match
        case Some(s) => Consequence.success(s)
        case None => Consequence.argumentInvalid(message)

    private def _record_get_as_c[A](
      record: Record,
      keys: List[String]
    )(using vr: org.goldenport.convert.ValueReader[A]): Consequence[Option[A]] =
      val normalized = ComponentFactory.normalizeDataStoreRecord(record)
      keys.foldLeft(Consequence.success(Option.empty[A])) { (z, key) =>
        z.flatMap {
          case s @ Some(_) => Consequence.success(s)
          case None => normalized.getAsC[A](key)
        }
      }

  private final case class ProvisionalRegistrationActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserProvisionalRegistration
  ) extends UserService.ProvisionalRegistrationActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      createUserAndCredential(action.record, Some("provisional"))

  private final case class RegisterActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserRegister
  ) extends UserService.RegisterActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      createUserAndCredential(action.record, Some("registered"))

  private final case class PromoteToFormalRegistrationActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserPromoteToFormalRegistration
  ) extends UserService.PromoteToFormalRegistrationActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        proofToken <- exec_from(requiredString(action.record, List("proofToken")))
        _ <- exec_from(ComponentFactory.requirePromotionProofToken(proofToken))
        response <- updateUserStatus(action.record, Some("formal"))
      yield
        response

  private final case class LoginActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserLogin
  ) extends UserService.LoginActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      login(action.record)

  private final case class LogoutActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserLogout
  ) extends UserService.LogoutActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      logout(action.record)

  private final case class LogoutAllActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserLogoutAll
  ) extends UserService.LogoutAllActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      logoutAll(action.record)

  private final case class RefreshAccessTokenActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserRefreshAccessToken
  ) extends UserService.RefreshAccessTokenActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      refreshAccessToken(action.record)

  private final case class LoadUserProfileViewActionCallImpl(
    core: ActionCall.Core,
    override val action: UserAccountComponent.ViewService.LoadUserProfileQuery
  ) extends UserAccountComponent.ViewService.LoadUserProfileActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        profile <- exec_from(
          EntityStore
            .standard()
            .load[UserProfileEntity](action.id)(using summon, ExecutionContext.withAggregateInternalRead(executionContext, true))
            .flatMap(x => Consequence.successOrEntityNotFound(x)(action.id))
        )
      yield
        OperationResponse(profile.toRecord())

  private final case class ChangePasswordActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserChangePassword
  ) extends UserService.ChangePasswordActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      changePassword(action.record)

  private final case class GetMyAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserGetMyAccount
  ) extends UserService.GetMyAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      getMyAccount(action.record)

  private final case class LookupUserByLoginNameActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserLookupByLoginName
  ) extends UserService.LookupUserByLoginNameActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      lookupUserByLoginName(action.record)

  private final case class VerifyMyEmailActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserVerifyMyEmail
  ) extends UserService.VerifyMyEmailActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      verifyMyEmail(action.record)

  private final case class VerifyMyPhoneActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserVerifyMyPhone
  ) extends UserService.VerifyMyPhoneActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      verifyMyPhone(action.record)

  private final case class RequestPasswordResetActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserRequestPasswordReset
  ) extends UserService.RequestPasswordResetActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      requestPasswordReset(action.record)

  private final case class ConfirmPasswordResetActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserConfirmPasswordReset
  ) extends UserService.ConfirmPasswordResetActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      confirmPasswordReset(action.record)

  private final case class EnrollTwoFactorActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserEnrollTwoFactor
  ) extends UserService.EnrollTwoFactorActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      _enroll_two_factor(action.record)

  private final case class VerifyTwoFactorLoginActionCallImpl(
    core: ActionCall.Core,
    override val action: UserService.UserVerifyTwoFactorLogin
  ) extends UserService.VerifyTwoFactorLoginActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      verifyTwoFactorLogin(action.record)

  private final case class ManagementCreateUserAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.AdminCreateUserAccount
  ) extends ManagementService.CreateUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- createUserAndCredential(action.record, Some("formal"))
      yield
        response

  private final case class ManagementUpdateUserStatusActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.AdminUpdateUserStatus
  ) extends ManagementService.UpdateUserStatusActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- updateUserStatus(action.record, None)
      yield
        response

  private final case class ManagementDeleteUserAccountActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.AdminDeleteUserAccount
  ) extends ManagementService.DeleteUserAccountActionCall, UserAccountActionSupport:
    protected def build_Program: ExecUowM[OperationResponse] =
      for
        _ <- exec_from(requireManagementPrivilege())
        response <- deleteUserAccount(action.record)
      yield
        response

  private final case class ManagementListUserAccountsActionCallImpl(
    core: ActionCall.Core,
    override val action: ManagementService.AdminListUserAccounts
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

  private final case class PasswordResetToken(
    token: String,
    userId: EntityId,
    email: String,
    handle: String,
    expiresAt: Instant,
    usedAt: Option[Instant] = None
  )

  private final case class TwoFactorChallenge(
    challengeId: String,
    userId: EntityId,
    email: String,
    handle: String,
    code: String,
    expiresAt: Instant,
    consumedAt: Option[Instant] = None
  )

  private val _password_reset_ttl_seconds: Int = 60 * 30
  private val _two_factor_challenge_ttl_seconds: Int = 60 * 10
  private val _password_reset_tokens = TrieMap.empty[String, PasswordResetToken]
  private val _two_factor_enrollments = TrieMap.empty[String, String]
  private val _two_factor_login_challenges = TrieMap.empty[String, TwoFactorChallenge]

  val AccessSessionTtlSeconds: Int = 60 * 60 * 8
  val RefreshSessionTtlSeconds: Int = 60 * 60 * 24 * 14

  def normalizeDataStoreRecord(record: Record): Record =
    _normalize_record_keys(record).TAKE

  private def _data_store_update_record(record: Record): Record =
    Record(record.fields.map { field =>
      field.copy(key = _storage_property_name(field.key))
    })

  private def _normalize_record_keys(record: Record): Consequence[Record] = {
    val normalized = record.fields.map { field =>
      _canonical_property_name(field.key) -> field
    }
    val collisions = normalized
      .groupBy(_._1)
      .collect {
        case (key, values) if values.lengthCompare(1) > 0 =>
          key -> values.map(_._2.key).distinct
      }
      .filter(_._2.lengthCompare(1) > 0)
    if (collisions.nonEmpty) {
      val message = collisions.toVector.sortBy(_._1).map {
        case (key, aliases) => s"$key: ${aliases.mkString(", ")}"
      }.mkString("; ")
      Consequence.argumentInvalid(s"Duplicate property aliases after canonical naming: $message")
    } else {
      Consequence.success(Record(normalized.map {
        case (key, field) => field.copy(key = key)
      }))
    }
  }

  private def _canonical_property_name(name: String): String = {
    val trimmed = name.trim
    if (trimmed.isEmpty)
      trimmed
    else if (!trimmed.exists(c => c == '_' || c == '-' || c == '.' || c.isWhitespace))
      trimmed
    else {
      val parts = trimmed
        .split("[_\\-\\.\\s]+")
        .toVector
        .filter(_.nonEmpty)
        .map(_.toLowerCase(java.util.Locale.ROOT))
      parts.headOption match {
        case None => ""
        case Some(head) =>
          head + parts.tail.map { x =>
            if (x.isEmpty) x else s"${x.head.toUpper}${x.tail}"
          }.mkString
      }
    }
  }

  private def _storage_property_name(name: String): String = {
    val canonical = _canonical_property_name(name)
    val b = new StringBuilder(canonical.length + 8)
    var i = 0
    while (i < canonical.length) {
      val c = canonical.charAt(i)
      if (c.isUpper) {
        if (b.nonEmpty)
          b.append('_')
        b.append(c.toLower)
      } else {
        b.append(c)
      }
      i += 1
    }
    b.result()
  }

  def authorizeOwnerOrManagerUserAccount(
    record: Record,
    access: CmlOperationAccess
  )(using ctx: ExecutionContext): Consequence[Unit] =
    for
      userId <- _resolve_target_user_id(record, access)
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

  private def _required_entity_id(
    record: Record,
    keys: Seq[String]
  ): Consequence[EntityId] =
    val normalized = normalizeDataStoreRecord(record)
    keys.iterator.map(normalized.getString).collectFirst {
      case Some(s) => summon[org.goldenport.convert.ValueReader[EntityId]].readC(s)
    }.getOrElse {
      Consequence.argumentMissingInput(keys)
    }

  private def _resolve_target_user_id(
    record: Record,
    access: CmlOperationAccess
  )(using ctx: ExecutionContext): Consequence[EntityId] =
    _required_entity_id(record, access.target.toList ++ List("userAccountId", "id")) match
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
          .withSuspendedAt(Instant.now)
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
      Consequence.stateConflict(s"Status ${p.value} is not allowed for account creation.")

  def requireAuthenticatable(p: UserAccountStatus): Consequence[Unit] =
    if (_authenticatable_statuses.contains(p))
      Consequence.unit
    else
      Consequence.securityPermissionDenied(s"Status ${p.value} cannot authenticate.")

  def requireTransition(current: UserAccountStatus, target: UserAccountStatus): Consequence[Unit] =
    if (current == target)
      Consequence.unit
    else if (_transitions.getOrElse(current, Set.empty).contains(target))
      Consequence.unit
    else
      Consequence.stateConflict(s"Status transition ${current.value} -> ${target.value} is not allowed.")

  def requirePromotionProofToken(token: String): Consequence[Unit] =
    if (token.trim.nonEmpty)
      Consequence.unit
    else
      Consequence.argumentInvalid("Promotion proof token must not be empty.")

  def requireEmailVerificationPending(user: Record): Consequence[Unit] =
    val normalized = normalizeDataStoreRecord(user)
    normalized.getString("emailVerifiedAt") match
      case Some(v) if v.trim.nonEmpty => Consequence.stateConflict("Current account email is already verified.")
      case _ => Consequence.unit

  def requirePhoneVerificationPending(
    user: Record,
    requestedPhoneNumber: String
  ): Consequence[Unit] =
    val normalized = normalizeDataStoreRecord(user)
    normalized.getString("phoneNumber") match
      case None | Some("") =>
        Consequence.stateConflict("Current account does not have an SMS contact to verify.")
      case Some(phone) if phone != requestedPhoneNumber =>
        Consequence.argumentInvalid(s"Requested phone number does not match the current account SMS contact: $requestedPhoneNumber")
      case Some(_) =>
        normalized.getString("phoneVerifiedAt") match
          case Some(v) if v.trim.nonEmpty => Consequence.stateConflict("Current account phone number is already verified.")
          case _ => Consequence.unit

  def requireEmailAvailable(
    email: String,
    _users: Vector[UserAccountEntity]
  ): Consequence[Unit] =
    if (_users.isEmpty)
      Consequence.unit
    else
      Consequence.stateConflict(s"User account email is already registered: $email")

  def requireLoginNameAvailable(
    loginname: String,
    _users: Vector[UserAccountEntity]
  ): Consequence[Unit] =
    if (_users.isEmpty)
      Consequence.unit
    else
      Consequence.stateConflict(s"User account loginName is already registered: $loginname")

  def requireLoggedIn(id: EntityId)(using ExecutionContext): Consequence[Unit] =
    currentLoggedInUserId(id).map(_ => ())

  def requireManagementPrivilege(using ctx: ExecutionContext): Consequence[Unit] =
    OperationAccessPolicy.authorizeManagerOnly()

  def currentLoggedInUserId()(using ctx: ExecutionContext): Consequence[EntityId] =
    _current_logged_in_user_id_from_session_id()
      .orElse(_current_logged_in_user_id_from_access_token())
      .orElse(_current_logged_in_user_id_from_principal_attribute())
      .orElse(LoginWorkingSet._current_user_id())

  def currentLoggedInUserId(id: EntityId)(using ctx: ExecutionContext): Consequence[EntityId] =
    currentLoggedInUserId().flatMap { current =>
      if (current == id)
        Consequence.success(id)
      else
        Consequence.securityPermissionDenied(s"Current authenticated user does not match target user account: ${id.print}")
    }

  def generateToken(): String =
    UUID.randomUUID().toString.replace("-", "") + UUID.randomUUID().toString.replace("-", "")

  private def _current_logged_in_user_id_from_session_id()(using ctx: ExecutionContext): Consequence[EntityId] =
    ctx.security.session.flatMap(_.sessionId).filter(_.trim.nonEmpty) match
      case Some(sessionId) =>
        _restore_access_session_by_id(sessionId).map(_.userAccountId)
      case None =>
        Consequence.securityAuthenticationRequired("No session id is available in security context.")

  private def _current_logged_in_user_id_from_access_token()(using ctx: ExecutionContext): Consequence[EntityId] =
    ctx.security.principal.attributes.get("accessToken")
      .filter(_.trim.nonEmpty) match
      case Some(token) =>
        _active_access_session_by_token(token).map(_.userAccountId)
      case None =>
        Consequence.securityAuthenticationRequired("No access token is available in security context.")

  private def _current_logged_in_user_id_from_principal_attribute()(using ctx: ExecutionContext): Consequence[EntityId] =
    List("userAccountId", "authorAccountId").view
      .flatMap(key => ctx.security.principal.attributes.get(key).filter(_.trim.nonEmpty))
      .headOption match
        case Some(value) => _principal_entity_id(value)
        case None => Consequence.securityAuthenticationRequired("No user account id is available in security context.")

  private def _principal_entity_id(value: String): Consequence[EntityId] =
    EntityId.parse(value).orElse {
      value.split("-entity-", 2).toList match
        case majorMinor :: rest :: Nil =>
          val major = majorMinor.takeWhile(_ != '-')
          val minor = majorMinor.drop(major.length + 1)
          val tail = rest.split("-", 4).toList
          tail match
            case collection :: timestamp :: entropy :: _ =>
              Consequence.success(EntityId(
                major,
                minor,
                EntityCollectionId(major, minor, collection),
                scala.util.Try(java.time.Instant.ofEpochMilli(timestamp.toLong)).toOption,
                Some(entropy)
              ))
            case _ =>
              Consequence.valueFormatError(s"Invalid EntityId value: $value")
        case _ =>
          Consequence.valueFormatError(s"Invalid EntityId value: $value")
    }

  private def _active_access_session_by_token(
    token: String
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    val hashed = _token_hash(token)
    _raw_access_sessions_by_token_hash(hashed).flatMap { sessions =>
      sessions.find(_is_active_access_session) match
        case Some(session) => Consequence.success(session)
        case None => Consequence.securityAuthenticationRequired("No active access session matches the current access token.")
    }

  private def _raw_access_sessions_by_token_hash(
    hashedToken: String
  )(using ctx: ExecutionContext): Consequence[Vector[AccessSessionEntity]] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      sessions <- _normalize_data_store_records(result.records.toVector)
        .filter(_.getString("tokenHash").contains(hashedToken))
        .traverse(AccessSessionEntity.createC)
    yield
      sessions

  private def _is_active_access_session(session: AccessSessionEntity): Boolean =
    session.revokedAt.isEmpty && !_is_expired(session.expiresAt)

  private def _is_revoked_access_session(session: AccessSessionEntity): Boolean =
    session.revokedAt.nonEmpty

  def isActiveRefreshSession(session: RefreshSessionEntity): Boolean =
    session.revokedAt.isEmpty && session.rotatedAt.isEmpty && !_is_expired(session.expiresAt)

  private def _is_reused_refresh_session(session: RefreshSessionEntity): Boolean =
    !_is_expired(session.expiresAt) && session.rotatedAt.nonEmpty

  private def _is_expired(expiresAt: Instant): Boolean =
    expiresAt.isBefore(Instant.now)

  private def _token_hash(token: String): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = digest.digest(token.getBytes(StandardCharsets.UTF_8))
    bytes.map(b => f"${b & 0xff}%02x").mkString

  def userAccountAuthenticationProvider(component: Component): AuthenticationProvider =
    new UserAccountAuthenticationProvider(component)

  private def _issue_password_reset_token(
    userId: EntityId,
    email: String,
    handle: String
  ): Consequence[PasswordResetToken] =
    val token = generateToken()
    val issued = PasswordResetToken(
      token = token,
      userId = userId,
      email = email,
      handle = handle,
      expiresAt = Instant.now.plusSeconds(_password_reset_ttl_seconds.toLong)
    )
    _password_reset_tokens.update(token, issued)
    Consequence.success(issued)

  private def _consume_password_reset_token(token: String): Consequence[PasswordResetToken] =
    _password_reset_tokens.get(token) match
      case Some(current) if current.usedAt.isEmpty && current.expiresAt.isAfter(Instant.now) =>
        val consumed = current.copy(usedAt = Some(Instant.now))
        _password_reset_tokens.update(token, consumed)
        Consequence.success(consumed)
      case Some(_) =>
        Consequence.securityPermissionDenied("invalid password reset token")
      case None =>
        Consequence.securityPermissionDenied("invalid password reset token")

  private def _enroll_two_factor(
    userId: EntityId,
    email: String,
    handle: String
  ): Consequence[Unit] =
    _two_factor_enrollments.update(userId.print, s"$email::$handle")
    Consequence.unit

  def isTwoFactorEnrolled(userId: EntityId): Boolean =
    _two_factor_enrollments.contains(userId.print)

  private def _issue_two_factor_login_challenge(
    userId: EntityId,
    email: String,
    handle: String
  ): Consequence[TwoFactorChallenge] =
    val challenge = TwoFactorChallenge(
      challengeId = UUID.randomUUID.toString,
      userId = userId,
      email = email,
      handle = handle,
      code = _verification_code(),
      expiresAt = Instant.now.plusSeconds(_two_factor_challenge_ttl_seconds.toLong)
    )
    _two_factor_login_challenges.update(challenge.challengeId, challenge)
    Consequence.success(challenge)

  private def _verify_two_factor_login_challenge(
    challengeId: String,
    code: String
  ): Consequence[TwoFactorChallenge] =
    _two_factor_login_challenges.get(challengeId) match
      case Some(challenge) if challenge.consumedAt.isEmpty && challenge.expiresAt.isAfter(Instant.now) && challenge.code == code =>
        val consumed = challenge.copy(consumedAt = Some(Instant.now))
        _two_factor_login_challenges.update(challengeId, consumed)
        Consequence.success(consumed)
      case Some(_) =>
        Consequence.securityPermissionDenied("invalid two-factor challenge")
      case None =>
        Consequence.securityPermissionDenied("invalid two-factor challenge")

  def resetEphemeralSecurityStateForTest(): Unit =
    _password_reset_tokens.clear()
    _two_factor_enrollments.clear()
    _two_factor_login_challenges.clear()

  private final class UserAccountAuthenticationProvider(component: Component) extends AuthenticationProvider {
    val name: String = "textus-user-account"

    def authenticate(request: AuthenticationRequest)(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
      _request_session_id(request) match
        case Some(sessionId) =>
          _optional_session_authenticate(sessionId)
        case None =>
          request.accessToken match
            case Some(token) =>
              authenticateAccessToken(token).map(Some(_))
            case None =>
              request.refreshToken match
                case Some(token) => authenticateRefreshToken(token).map(Some(_))
                case None => Consequence.success(None)

    override def login(request: AuthenticationRequest)(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
      _provider_login(component, request)

    override def logout(request: AuthenticationRequest)(using ctx: ExecutionContext): Consequence[Option[SessionContext]] =
      _request_session_id(request) match
        case Some(sessionId) =>
          _optional_session_logout(sessionId)
        case None =>
          Consequence.success(None)

    override def currentSession(request: AuthenticationRequest)(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
      _request_session_id(request) match
        case Some(sessionId) =>
          _optional_session_authenticate(sessionId)
        case None =>
          authenticate(request)
  }

  private def _optional_session_authenticate(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
    authenticateSessionId(sessionId) match
      case Consequence.Success(result) =>
        Consequence.success(Some(result))
      case m: Consequence.Failure[?] if _is_missing_or_invalid_session(m) =>
        Consequence.success(None)
      case m: Consequence.Failure[?] =>
        m.asInstanceOf[Consequence[Option[AuthenticationResult]]]

  private def _optional_session_logout(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[Option[SessionContext]] =
    logoutSessionId(sessionId) match
      case Consequence.Success(result) =>
        Consequence.success(Some(result))
      case m: Consequence.Failure[?] if _is_missing_or_invalid_session(m) =>
        Consequence.success(None)
      case m: Consequence.Failure[?] =>
        m.asInstanceOf[Consequence[Option[SessionContext]]]

  private def _is_missing_or_invalid_session(
    m: Consequence.Failure[?]
  ): Boolean = {
    val shown = m.conclusion.show
    shown.contains("Entity.NotFound[") || shown.contains("invalid session") || shown.contains("Invalid UniversalId format")
  }

  private def _provider_login(
    component: Component,
    request: AuthenticationRequest
  )(using ctx: ExecutionContext): Consequence[Option[AuthenticationResult]] =
    for
      identifier <- _required_login_identifier(request)
      password <- _required_login_password(request)
      email <- _resolve_login_email(identifier)
      response <- _execute_request(
        component,
        _anonymous_execution_context(ctx),
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("email", email, None),
            Property("password", password, None)
          )
        )
      )
      result <- _provider_login_result(response)
    yield
      Some(result)

  private def _anonymous_execution_context(
    ctx: ExecutionContext
  ): ExecutionContext =
    ExecutionContext.withSecurityContext(
      ctx,
      SecurityContext(
        principal = new Principal {
          val id: PrincipalId = SecurityContext.Privilege.Anonymous.principalId
          val attributes: Map[String, String] = SecurityContext.Privilege.Anonymous.attributes
        },
        capabilities = SecurityContext.Privilege.Anonymous.capabilities,
        level = SecurityContext.Privilege.Anonymous.level,
        subjectKind = SecurityContext.Privilege.Anonymous.subjectKind
      )
    )

  private def _request_session_id(
    request: AuthenticationRequest
  ): Option[String] =
    Vector(
      "x-textus-session",
      "sessionId",
      "textus.session"
    ).iterator.flatMap(request.attribute).collectFirst {
      case value if value.trim.nonEmpty => value.trim
    }

  private def _required_login_identifier(
    request: AuthenticationRequest
  ): Consequence[String] =
    Vector("email", "username", "loginName")
      .iterator
      .flatMap(request.attribute)
      .collectFirst {
        case value if value.trim.nonEmpty => value.trim
      }
      .map(Consequence.success)
      .getOrElse(Consequence.argumentMissingInput("login identifier"))

  private def _required_login_password(
    request: AuthenticationRequest
  ): Consequence[String] =
    request.attribute("password")
      .filter(_.trim.nonEmpty)
      .map(_.trim)
      .map(Consequence.success)
      .getOrElse(Consequence.argumentMissingInput("password"))

  private def _resolve_login_email(
    identifier: String
  )(using ctx: ExecutionContext): Consequence[String] =
    if (identifier.contains("@"))
      Consequence.success(identifier)
    else
      _raw_user_accounts_by_login_name(identifier).map {
        _.headOption.map(_.email).getOrElse(identifier)
      }

  private def _provider_login_result(
    response: OperationResponse
  )(using ctx: ExecutionContext): Consequence[AuthenticationResult] =
    response match
      case RecordResponse(record) =>
        record.getString("accessSessionId") match
          case Some(sessionId) if sessionId.trim.nonEmpty =>
            authenticateSessionId(sessionId.trim)
          case _ if record.getString("challengeId").nonEmpty =>
            Consequence.securityPermissionDenied("two-factor challenge required")
          case _ =>
            Consequence.operationInvalid("login", "textus-user-account login did not return accessSessionId")
      case other =>
        Consequence.operationInvalid("login", s"textus-user-account login returned unexpected response: ${other.getClass.getSimpleName}")

  private def _execute_request(
    component: Component,
    ctx: ExecutionContext,
    request: Request
  ): Consequence[OperationResponse] =
    component.logic.makeOperationRequest(request).flatMap {
      case action: org.goldenport.cncf.action.Action =>
        val call = component.logic.createActionCall(action, ctx)
        component.actionEngine.execute(call)
      case other =>
        Consequence.operationInvalid("request", s"request did not resolve to action: $other")
    }

  def authenticateSessionId(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[AuthenticationResult] =
    for
      session <- _restore_access_session_by_id(sessionId)
      user <- _load_user_account(session.userAccountId)
    yield
      _authentication_result(user, session)

  def logoutSessionId(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[SessionContext] =
    for
      session <- _load_access_session_by_id(sessionId)
      _ <- if (_is_revoked_access_session(session))
        Consequence.securityAuthenticationRequired("invalid session")
      else
        Consequence.unit
      _ <- _revoke_access_session_direct(session.id)
      _ <- _revoke_linked_refresh_session(session)
    yield
      SessionContext(sessionId = Some(session.id.print), tokenId = Some(session.id.print), tokenKind = Some("access"))

  private def _restore_access_session_by_id(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    for
      session <- _load_access_session_by_id(sessionId)
      restored <-
        if (_is_active_access_session(session))
          _touch_access_session(session.id).map(_ => session.copy(lastAccessedAt = Some(Instant.now)))
        else if (_is_revoked_access_session(session))
          Consequence.securityAuthenticationRequired("invalid session")
        else
          _rotate_internal_session_state(session)
    yield
      restored

  private def _rotate_internal_session_state(
    session: AccessSessionEntity
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    session.refreshSessionId match
      case Some(refreshIdText) =>
        for
          refreshId <- EntityId.parse(refreshIdText)
          refresh <- _load_refresh_session(refreshId)
          _ <- if (isActiveRefreshSession(refresh)) Consequence.unit else Consequence.securityAuthenticationRequired("invalid session")
          successor <- _issue_refresh_session_direct(
            refresh.userAccountId,
            None,
            refresh.clientId,
            refresh.deviceInfo,
            refresh.ipAddress,
            refresh.userAgent,
            predecessor = Some(refresh.id)
          )
          now = Instant.now
          _ <- _update_refresh_session_fields_direct(
            refresh.id,
            Record.dataAuto(
              "revokedAt" -> now.toString,
              "rotatedAt" -> now.toString,
              "successorSessionId" -> successor.id.print
            )
          )
          _ <- _update_access_session_fields_direct(
            session.id,
            Record.dataAuto(
              "refreshSessionId" -> successor.id.print,
              "tokenHash" -> _token_hash(generateToken()),
              "issuedAt" -> now.toString,
              "expiresAt" -> now.plusSeconds(AccessSessionTtlSeconds.toLong).toString,
              "lastAccessedAt" -> now.toString,
              "clientId" -> refresh.clientId,
              "deviceInfo" -> refresh.deviceInfo,
              "ipAddress" -> refresh.ipAddress,
              "userAgent" -> refresh.userAgent
            )
          )
          restored <- _load_access_session(session.id)
        yield
          restored
      case None =>
        Consequence.securityAuthenticationRequired("invalid session")

  private def _load_access_session_by_id(
    sessionId: String
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    for
      id <- EntityId.parse(sessionId)
      session <- _load_access_session(id)
    yield
      session

  private def _load_access_session(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    EntityStore.standard().load[AccessSessionEntity](sessionId).flatMap {
      case Some(session) => Consequence.success(session)
      case None => _load_access_session_from_raw(sessionId)
    }

  private def _load_refresh_session(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[RefreshSessionEntity] =
    EntityStore.standard().load[RefreshSessionEntity](sessionId).flatMap {
      case Some(session) => Consequence.success(session)
      case None => _load_refresh_session_from_raw(sessionId)
    }

  private def _load_access_session_from_raw(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[AccessSessionEntity] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      record <- Consequence.successOrEntityNotFound(
        _normalize_data_store_records(result.records.toVector).find(_record_entity_id_matches(_, sessionId))
      )(sessionId)
      session <- AccessSessionEntity.createC(record)
    yield
      session

  private def _load_refresh_session_from_raw(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[RefreshSessionEntity] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      record <- Consequence.successOrEntityNotFound(
        _normalize_data_store_records(result.records.toVector).find(_record_entity_id_matches(_, sessionId))
      )(sessionId)
      session <- RefreshSessionEntity.createC(record)
    yield
      session

  private def _issue_refresh_session_direct(
    userId: EntityId,
    requestedToken: Option[String],
    clientId: Option[String],
    deviceInfo: Option[String],
    ipAddress: Option[String],
    userAgent: Option[String],
    predecessor: Option[EntityId] = None
  )(using ctx: ExecutionContext): Consequence[RefreshSessionEntity] =
    for
      now <- Consequence.success(Instant.now)
      rawToken <- Consequence.success(requestedToken.filter(_.trim.nonEmpty).getOrElse(generateToken()))
      sessionRecord = Record.dataAuto(
        "userAccountId" -> userId,
        "tokenHash" -> _token_hash(rawToken),
        "issuedAt" -> now.toString,
        "expiresAt" -> now.plusSeconds(RefreshSessionTtlSeconds.toLong).toString,
        "clientId" -> clientId,
        "deviceInfo" -> deviceInfo,
        "ipAddress" -> ipAddress,
        "userAgent" -> userAgent
      )
      session0 <- RefreshSessionCreate.createC(sessionRecord)
      session = session0.withResourceAttributes(ResourceAttributes(activationStatus = ActivationStatus.Active))
      created <- EntityStore.standard().create(session)
      _ <- predecessor match
        case Some(previousId) =>
          _update_refresh_session_fields_direct(
            previousId,
            Record.dataAuto("successorSessionId" -> created.id.print)
          )
        case None =>
          Consequence.unit
      stored <- _load_refresh_session(created.id)
    yield
      stored

  private def _touch_access_session(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[Unit] =
    _update_access_session_fields_direct(
      sessionId,
      Record.dataAuto("lastAccessedAt" -> Instant.now)
    )

  private def _revoke_access_session_direct(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[Unit] =
    _update_access_session_fields_direct(
      sessionId,
      Record.dataAuto(
        "revokedAt" -> Instant.now,
        "lastAccessedAt" -> Instant.now
      )
    )

  private def _revoke_refresh_session_direct(
    sessionId: EntityId
  )(using ctx: ExecutionContext): Consequence[Unit] =
    _update_refresh_session_fields_direct(
      sessionId,
      Record.dataAuto("revokedAt" -> Instant.now)
    )

  private def _revoke_linked_refresh_session(
    accessSession: AccessSessionEntity
  )(using ctx: ExecutionContext): Consequence[Unit] =
    accessSession.refreshSessionId match
      case Some(refreshIdText) =>
        for
          refreshId <- EntityId.parse(refreshIdText)
          _ <- _revoke_refresh_session_direct(refreshId)
        yield
          ()
      case None =>
        Consequence.unit

  private def _update_refresh_session_fields_direct(
    sessionId: EntityId,
    changes: Record
  )(using ctx: ExecutionContext): Consequence[Unit] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId)
      ds <- ctx.dataStoreSpace.dataStore(cid)
      _ <- ds.update(
        cid,
        org.goldenport.cncf.datastore.DataStore.EntryId(sessionId),
        _data_store_update_record(changes)
      )
    yield
      ()

  private def _update_access_session_fields_direct(
    sessionId: EntityId,
    changes: Record
  )(using ctx: ExecutionContext): Consequence[Unit] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId)
      ds <- ctx.dataStoreSpace.dataStore(cid)
      _ <- ds.update(
        cid,
        org.goldenport.cncf.datastore.DataStore.EntryId(sessionId),
        _data_store_update_record(changes)
      )
    yield
      ()

  private def _raw_user_accounts_by_login_name(
    loginName: String
  )(using ctx: ExecutionContext): Consequence[Vector[UserAccountEntity]] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      _users <- _normalize_data_store_records(result.records.toVector)
        .filter(_.getString("loginName").contains(loginName))
        .traverse(UserAccountEntity.createC)
    yield
      _users

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
    EntityStore.standard().load[UserAccountEntity](userId).flatMap {
      case Some(user) => Consequence.success(user)
      case None => _load_user_account_from_raw(userId)
    }

  private def _load_user_account_from_raw(
    userId: EntityId
  )(using ctx: ExecutionContext): Consequence[UserAccountEntity] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      record <- Consequence.successOrEntityNotFound(
        _normalize_data_store_records(result.records.toVector).find(_record_entity_id_matches(_, userId))
      )(userId)
      user <- UserAccountEntity.createC(record)
    yield
      user

  private def _active_refresh_session_by_token(
    token: String
  )(using ctx: ExecutionContext): Consequence[RefreshSessionEntity] =
    val hashed = _token_hash(token)
    _raw_refresh_sessions_by_token_hash(hashed).flatMap { sessions =>
      sessions.find(isActiveRefreshSession) match
        case Some(session) => Consequence.success(session)
        case None => Consequence.securityAuthenticationRequired("No active refresh session matches the current refresh token.")
    }

  private def _raw_refresh_sessions_by_token_hash(
    hashedToken: String
  )(using ctx: ExecutionContext): Consequence[Vector[RefreshSessionEntity]] =
    for
      cid <- ctx.entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId)
      result <- ctx.dataStoreSpace.search(cid, DsQueryDirective(DsQuery.Empty))
      sessions <- _normalize_data_store_records(result.records.toVector)
        .filter(_.getString("tokenHash").contains(hashedToken))
        .traverse(RefreshSessionEntity.createC)
    yield
      sessions

  private def _record_entity_id_matches(record: Record, id: EntityId): Boolean =
    record.getAsC[EntityId]("id").toOption.flatten.exists(_ == id) ||
      record.getString("id").contains(id.print)

  private def _normalize_data_store_records(records: Vector[Record]): Vector[Record] =
    records.map(_normalize_data_store_record)

  private def _normalize_data_store_record(record: Record): Record =
    normalizeDataStoreRecord(record)

  private def _authentication_result(
    user: UserAccountEntity,
    session: AccessSessionEntity,
    accessToken: String
  ): AuthenticationResult =
    AuthenticationResult(
      principalId = PrincipalId(user.id.print),
      attributes = Map(
        "userAccountId" -> user.id.print,
        "role" -> "user",
        "privilege" -> "user",
        "email" -> user.email,
        "loginName" -> user.loginName.orNull,
        "handle" -> user.loginName.orNull,
        "locale" -> user.locale.orNull,
        "timeZone" -> user.timeZone.orNull,
        "shortid" -> user.id.parts.entropy,
        "accessToken" -> accessToken
      ).collect { case (k, v) if v != null && v.nonEmpty => k -> v },
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
          attributes = _session_attributes(session.clientId, session.deviceInfo, session.ipAddress, session.userAgent) ++ _session_display_attributes(user.locale, user.timeZone)
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
        "userAccountId" -> user.id.print,
        "role" -> "user",
        "privilege" -> "user",
        "email" -> user.email,
        "loginName" -> user.loginName.orNull,
        "handle" -> user.loginName.orNull,
        "locale" -> user.locale.orNull,
        "timeZone" -> user.timeZone.orNull,
        "shortid" -> user.id.parts.entropy,
        "refreshToken" -> refreshToken
      ).collect { case (k, v) if v != null && v.nonEmpty => k -> v },
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
          attributes = _session_attributes(session.clientId, session.deviceInfo, session.ipAddress, session.userAgent) ++ _session_display_attributes(user.locale, user.timeZone)
        )
      )
    )


  private def _authentication_result(
    user: UserAccountEntity,
    session: AccessSessionEntity
  ): AuthenticationResult =
    AuthenticationResult(
      principalId = PrincipalId(user.id.print),
      attributes = Map(
        "userAccountId" -> user.id.print,
        "role" -> "user",
        "privilege" -> "user",
        "email" -> user.email,
        "loginName" -> user.loginName.orNull,
        "handle" -> user.loginName.orNull,
        "locale" -> user.locale.orNull,
        "timeZone" -> user.timeZone.orNull,
        "shortid" -> user.id.parts.entropy
      ).collect { case (k, v) if v != null && v.nonEmpty => k -> v },
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
          attributes = _session_attributes(session.clientId, session.deviceInfo, session.ipAddress, session.userAgent) ++ _session_display_attributes(user.locale, user.timeZone)
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
      clientId.map("clientId" -> _),
      deviceInfo.map("deviceInfo" -> _),
      ipAddress.map("ipAddress" -> _),
      userAgent.map("userAgent" -> _)
    ).flatten.toMap

  private def _session_display_attributes(
    locale: Option[String],
    timeZone: Option[String]
  ): Map[String, String] =
    Vector(
      locale.map("locale" -> _),
      timeZone.map("timeZone" -> _)
    ).flatten.toMap

  private def _parse_instant(p: Instant): Option[Instant] =
    Some(p)

  private def _verification_code(): String =
    f"${Random.nextInt(1000000)}%06d"

  private object LoginWorkingSet:
    private val _users = TrieMap.empty[EntityId, UserAccountEntity]

    def upsert(user: UserAccountEntity): Unit =
      _users.put(user.id, user)

    def remove(id: EntityId): Unit =
      _users.remove(id)

    def contains(id: EntityId): Boolean =
      _users.contains(id)

    def _current_user_id(): Consequence[EntityId] =
      _users.keys.toVector match
        case Vector(id) => Consequence.success(id)
        case Vector() => Consequence.stateConflict("No active user account is available in working set.")
        case _ => Consequence.stateConflict("Multiple active user accounts are available in working set.")

    def snapshot: Vector[UserAccountEntity] =
      _users.values.toVector

    def clear(): Unit =
      _users.clear()

  private[textus] def resetLoginWorkingSetForTest(): Unit =
    LoginWorkingSet.clear()

  private[textus] def addLoggedInUserForTest(userId: EntityId)(using ExecutionContext): Consequence[Unit] =
    EntityStore.standard().load[UserAccountEntity](userId).flatMap {
      case Some(user) =>
        LoginWorkingSet.upsert(user)
        Consequence.unit
      case None =>
        Consequence.entityNotFound(s"User account not found for working set: ${userId.print}")
    }

  def create(componentCreate: ComponentCreate): Vector[Component] =
    Vector(_with_artifact_metadata(new ComponentFactory().createPrimary(componentCreate)))

  def createStandalone(): Component =
    _with_artifact_metadata(UserAccountComponent())

  private def _with_artifact_metadata(component: Component): Component =
    component.withArtifactMetadata(
      Component.ArtifactMetadata(
        sourceType = "standalone",
        name = "textus-user-account",
        version = CncfVersion.current,
        component = Some("textus-user-account")
      )
    )
