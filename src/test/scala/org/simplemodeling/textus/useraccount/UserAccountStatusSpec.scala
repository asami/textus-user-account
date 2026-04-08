package org.simplemodeling.textus.useraccount
import org.goldenport.Consequence
import cats.~>
import org.goldenport.configuration.{Configuration, ConfigurationTrace, ResolvedConfiguration}
import org.goldenport.cncf.action.{Action, ActionCall, ProcedureActionCall}
import org.goldenport.cncf.datastore.DataStore
import org.goldenport.cncf.entity.EntityStore
import org.goldenport.cncf.entity.EntityQuery
import org.goldenport.cncf.event.EventEngine
import org.goldenport.cncf.component.{Component, ComponentCreate, ComponentOrigin}
import org.goldenport.cncf.cli.RunMode
import org.goldenport.cncf.context.{ExecutionContext, Principal, PrincipalId, RuntimeContext, SecurityContext}
import org.goldenport.cncf.directive.Query
import org.goldenport.cncf.path.AliasResolver
import org.goldenport.cncf.subsystem.Subsystem
import org.goldenport.cncf.unitofwork.{CommitRecorder, UnitOfWork, UnitOfWorkInterpreter, UnitOfWorkOp}
import org.goldenport.datatype.Name
import org.goldenport.protocol.{Property, Request}
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.datatype.{Identifier, Name, ObjectId}
import org.simplemodeling.model.datatype.EntityId
import org.simplemodeling.model.directive.{Condition, Update}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.model.value.{AuditAttributes, ContextualAttributes, DescriptiveAttributes, LifecycleAttributes, MediaAttributes, NameAttributes, PublicationAttributes, ResourceAttributes, SecurityAttributes}
import org.simplemodeling.textus.useraccount.entity.{UserAccount => UserAccountEntity}
import org.simplemodeling.textus.useraccount.entity.create.{UserAccount => UserAccountCreateEntity}
import org.simplemodeling.textus.useraccount.entity.query.{UserAccount => UserAccountQuery}
import org.simplemodeling.textus.useraccount.entity.create.UserAccount.given

/*
 * @since   Apr.  6, 2026
 * @version Apr.  8, 2026
 * @author  ASAMI, Tomoharu
 */
final class UserAccountStatusSpec extends AnyWordSpec with Matchers {
  "ComponentFactory status policy" should {
    "parse declared states" in {
      ComponentFactory.parseStatus("provisional").toOption shouldBe Some(UserAccountStatus.Provisional)
      ComponentFactory.parseStatus("registered").toOption shouldBe Some(UserAccountStatus.Registered)
      ComponentFactory.parseStatus("formal").toOption shouldBe Some(UserAccountStatus.Formal)
      ComponentFactory.parseStatus("suspended").toOption shouldBe Some(UserAccountStatus.Suspended)
    }

    "decode datastore db values" in {
      summon[org.goldenport.convert.ValueReader[UserAccountStatus]].read(0) shouldBe Some(UserAccountStatus.Provisional)
      summon[org.goldenport.convert.ValueReader[UserAccountStatus]].read(1) shouldBe Some(UserAccountStatus.Registered)
      summon[org.goldenport.convert.ValueReader[UserAccountStatus]].read("2") shouldBe Some(UserAccountStatus.Formal)
      summon[org.goldenport.convert.ValueReader[UserAccountStatus]].read(3L) shouldBe Some(UserAccountStatus.Suspended)
    }

    "reject unknown states" in {
      ComponentFactory.parseStatus("active").toOption.isDefined shouldBe false
      ComponentFactory.parseStatus("deleted").toOption.isDefined shouldBe false
    }

    "allow only declared lifecycle transitions" in {
      ComponentFactory.requireTransition(UserAccountStatus.Provisional, UserAccountStatus.Formal).toOption.isDefined shouldBe true
      ComponentFactory.requireTransition(UserAccountStatus.Provisional, UserAccountStatus.Suspended).toOption.isDefined shouldBe true
      ComponentFactory.requireTransition(UserAccountStatus.Suspended, UserAccountStatus.Registered).toOption.isDefined shouldBe true

      ComponentFactory.requireTransition(UserAccountStatus.Registered, UserAccountStatus.Formal).toOption.isDefined shouldBe false
      ComponentFactory.requireTransition(UserAccountStatus.Formal, UserAccountStatus.Registered).toOption.isDefined shouldBe false
      ComponentFactory.requireTransition(UserAccountStatus.Suspended, UserAccountStatus.Formal).toOption.isDefined shouldBe false
    }

    "allow authentication only for loginable states" in {
      ComponentFactory.requireAuthenticatable(UserAccountStatus.Registered).toOption.isDefined shouldBe true
      ComponentFactory.requireAuthenticatable(UserAccountStatus.Formal).toOption.isDefined shouldBe true

      ComponentFactory.requireAuthenticatable(UserAccountStatus.Provisional).toOption.isDefined shouldBe false
      ComponentFactory.requireAuthenticatable(UserAccountStatus.Suspended).toOption.isDefined shouldBe false
    }

    "map account status into common activation status" in {
      ComponentFactory.activationStatusForUserAccountStatus(UserAccountStatus.Provisional) shouldBe org.simplemodeling.model.statemachine.ActivationStatus.Inactive
      ComponentFactory.activationStatusForUserAccountStatus(UserAccountStatus.Registered) shouldBe org.simplemodeling.model.statemachine.ActivationStatus.Active
      ComponentFactory.activationStatusForUserAccountStatus(UserAccountStatus.Formal) shouldBe org.simplemodeling.model.statemachine.ActivationStatus.Active
      ComponentFactory.activationStatusForUserAccountStatus(UserAccountStatus.Suspended) shouldBe org.simplemodeling.model.statemachine.ActivationStatus.Deactivated
    }

    "require a non-empty promotion proof token" in {
      ComponentFactory.requirePromotionProofToken("verified-email").toOption.isDefined shouldBe true
      ComponentFactory.requirePromotionProofToken("").toOption.isDefined shouldBe false
      ComponentFactory.requirePromotionProofToken("   ").toOption.isDefined shouldBe false
    }

    "require management privilege for management operations" in {
      {
        given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.User)
        ComponentFactory.requireManagementPrivilege.toOption.isDefined shouldBe false
      }

      {
        given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.ApplicationContentManager)
        ComponentFactory.requireManagementPrivilege.toOption.isDefined shouldBe true
      }
    }

    "use manager-only entity authorization for management operations" in {
      {
        given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.User)
        ComponentFactory.authorizeUserAccountEntityOperation("listUserAccounts", org.goldenport.record.Record(), "UserAccount").toOption.isDefined shouldBe false
      }

      {
        given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.ApplicationContentManager)
        ComponentFactory.authorizeUserAccountEntityOperation("listUserAccounts", org.goldenport.record.Record(), "UserAccount").toOption.isDefined shouldBe true
      }
    }

    "carry pagination controls into list query directives" in {
      val condition = org.simplemodeling.textus.useraccount.entity.query.UserAccount(
        id = Condition.any[EntityId],
        name = Condition.any[Name],
        title = Condition.any[String],
        email = Condition.any[String],
        emailVerifiedAt = Condition.any[String],
        phoneNumber = Condition.any[String],
        phoneVerifiedAt = Condition.any[String],
        lastLoginAt = Condition.any[String],
        passwordChangedAt = Condition.any[String],
        suspendedAt = Condition.any[String],
        suspendedBy = Condition.any[String],
        suspensionReason = Condition.any[String],
        status = Condition.any[UserAccountStatus]
      )
      val query = Query.plan(
        condition,
        limit = Some(10),
        offset = Some(20)
      )
      Query.offsetOf(query) shouldBe Some(20)
      Query.limitOf(query) shouldBe Some(10)
    }

    "reject duplicate account emails" in {
      val duplicated = Vector(null.asInstanceOf[org.simplemodeling.textus.useraccount.entity.UserAccount])
      ComponentFactory.requireEmailAvailable("alice@example.com", Vector.empty).toOption.isDefined shouldBe true
      ComponentFactory.requireEmailAvailable("alice@example.com", duplicated).toOption.isDefined shouldBe false
    }

    "resolve current user id from the active working set" in {
      ComponentFactory.currentLoggedInUserId().toOption.isDefined shouldBe false
    }

    "expose target entity metadata for changePassword" in {
      val entityName = UserAccountComponent().operationDefinitions.find(_.name == "changePassword").flatMap(_.entityName)
      entityName shouldBe Some("UserAccount")
    }

    "expose target entity metadata for getMyAccount" in {
      val entityName = UserAccountComponent().operationDefinitions.find(_.name == "getMyAccount").flatMap(_.entityName)
      entityName shouldBe Some("UserAccount")
    }

    "expose target entity metadata for management operations via service default" in {
      val operationDefinitions = UserAccountComponent().operationDefinitions
      operationDefinitions.find(_.name == "updateUserStatus").flatMap(_.entityName) shouldBe Some("UserAccount")
      operationDefinitions.find(_.name == "deleteUserAccount").flatMap(_.entityName) shouldBe Some("UserAccount")
      operationDefinitions.find(_.name == "listUserAccounts").flatMap(_.entityName) shouldBe Some("UserAccount")
    }

    "expose default access metadata for management operations via service policy" in {
      val operationDefinitions = UserAccountComponent().operationDefinitions
      operationDefinitions.find(_.name == "listUserAccounts").flatMap(_.access.map(_.policy)) shouldBe Some("manager_only")
      operationDefinitions.find(_.name == "updateUserStatus").flatMap(_.access.map(_.policy)) shouldBe Some("manager_only")
    }

    "expose authentication access metadata for user self-service operations" in {
      val operationDefinitions = UserAccountComponent().operationDefinitions
      operationDefinitions.find(_.name == "provisionalRegistration").flatMap(_.access.map(_.policy)) shouldBe Some("anonymous_only")
      operationDefinitions.find(_.name == "register").flatMap(_.access.map(_.policy)) shouldBe Some("anonymous_only")
      operationDefinitions.find(_.name == "login").flatMap(_.access.map(_.policy)) shouldBe Some("anonymous_only")
      operationDefinitions.find(_.name == "logout").flatMap(_.access.map(_.policy)) shouldBe Some("owner_or_manager")
      operationDefinitions.find(_.name == "changePassword").flatMap(_.access.map(_.policy)) shouldBe Some("owner_or_manager")
      operationDefinitions.find(_.name == "verifyMyEmail").flatMap(_.access.map(_.policy)) shouldBe Some("owner_or_manager")
      operationDefinitions.find(_.name == "verifyMyPhone").flatMap(_.access.map(_.policy)) shouldBe Some("owner_or_manager")
      operationDefinitions.find(_.name == "getMyAccount").flatMap(_.access.map(_.policy)) shouldBe Some("owner_or_manager")
    }

    "enforce anonymous-only authorization during direct action execution" in {
      val component = _component()
      val fixture = _runtime_fixture()
      val anonymousCtx = _execution_context(
        fixture,
        _security_context("anonymous", SecurityContext.Privilege.User, withAccessToken = false, extraAttributes = Map("anonymous" -> "true"))
      )
      val authenticatedCtx = _execution_context(
        fixture,
        _security_context("plain_user_principal", SecurityContext.Privilege.User)
      )

      _execute(
        component,
        anonymousCtx,
        _dummy_action(
          "User",
          "register",
          "email" -> "anon@example.com",
          "password" -> "secret"
        )
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        authenticatedCtx,
        _dummy_action(
          "User",
          "register",
          "email" -> "auth@example.com",
          "password" -> "secret"
        )
      ).toOption.isDefined shouldBe false
    }

    "enforce owner-or-manager authorization for logout during direct action execution" in {
      val component = _component()
      val fixture = _runtime_fixture()
      val userId = EntityId("test", "logout", UserAccountQuery.collectionId)
      val managerCtx = _execution_context(
        fixture,
        _security_context("manager_principal", SecurityContext.Privilege.ApplicationContentManager)
      )
      val anonymousCtx = _execution_context(
        fixture,
        _security_context("anonymous", SecurityContext.Privilege.User, withAccessToken = false, extraAttributes = Map("anonymous" -> "true"))
      )
      given ExecutionContext = managerCtx
      _seed_user_account(userId, "logout_principal", "logout@example.com").toOption.isDefined shouldBe true
      ComponentFactory.addLoggedInUserForTest(userId)(using managerCtx).toOption.isDefined shouldBe true

      _execute(
        component,
        managerCtx,
        _dummy_action("User", "logout", "userAccountId" -> userId.print)
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        anonymousCtx,
        _dummy_action("User", "logout", "userAccountId" -> userId.print)
      ).toOption.isDefined shouldBe false
    }

    "enforce owner-or-manager authorization during direct action execution" in {
      val component = _component()
      val fixture = _runtime_fixture()
      val userId = EntityId("test", s"owner_${System.nanoTime}", UserAccountQuery.collectionId)
      val ownerCtx = _execution_context(fixture, _security_context(userId.print, SecurityContext.Privilege.User))
      val otherCtx = _execution_context(fixture, _security_context("other_principal", SecurityContext.Privilege.User))
      given ExecutionContext = ownerCtx
      _seed_user_account(userId, "owner_principal", "owner@example.com").toOption.isDefined shouldBe true
      ComponentFactory.authorizeUserAccountEntityOperation(
        "changePassword",
        org.goldenport.record.Record.dataAuto("userAccountId" -> userId.print),
        "UserAccount"
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        ownerCtx,
        _dummy_action(
          "User",
          "changePassword",
          "userAccountId" -> userId.print,
          "currentPassword" -> "secret",
          "newPassword" -> "secret-2"
        )
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        otherCtx,
        _dummy_action(
          "User",
          "changePassword",
          "userAccountId" -> userId.print,
          "currentPassword" -> "secret",
          "newPassword" -> "blocked"
        )
      ).toOption.isDefined shouldBe false
    }

    "enforce manager-only authorization during direct action execution" in {
      val component = _component()
      val fixture = _runtime_fixture()
      val managerCtx = _execution_context(fixture, _security_context("manager_principal", SecurityContext.Privilege.ApplicationContentManager))
      val userCtx = _execution_context(fixture, _security_context("plain_user_principal", SecurityContext.Privilege.User))

      _execute(
        component,
        managerCtx,
        _dummy_action("Management", "listUserAccounts")
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        userCtx,
        _dummy_action("Management", "listUserAccounts")
      ).toOption.isDefined shouldBe false
    }

    "accept self-service verification actions for the current user" in {
      val component = _component()
      val fixture = _runtime_fixture()
      val userId = EntityId("test", s"verified_${System.nanoTime}", UserAccountQuery.collectionId)
      val ownerCtx = _execution_context(fixture, _security_context(userId.print, SecurityContext.Privilege.User))
      given ExecutionContext = ownerCtx
      _seed_user_account(userId, "verified_owner_principal", "verified@example.com").toOption.isDefined shouldBe true

      _execute(
        component,
        ownerCtx,
        _dummy_action(
          "User",
          "verifyMyEmail",
          "userAccountId" -> userId.print,
          "proofToken" -> "email-proof"
        )
      ).toOption.isDefined shouldBe true

      _execute(
        component,
        ownerCtx,
        _dummy_action(
          "User",
          "verifyMyPhone",
          "userAccountId" -> userId.print,
          "phoneNumber" -> "09012345678",
          "proofToken" -> "phone-proof"
        )
      ).toOption.isDefined shouldBe true
    }

    "build status update fields with synchronized activation status" in {
      given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.ApplicationContentManager)
      val changes = ComponentFactory.statusUpdateFields(UserAccountStatus.Suspended, Some("policy_violation"))
      changes.status shouldBe Update.set(UserAccountStatus.Suspended)
      changes.suspendedBy.isSet shouldBe true
      changes.suspensionReason shouldBe Update.set("policy_violation")
      changes.suspendedAt.isSet shouldBe true
      changes.resourceAttributes.activationStatus shouldBe Update.set(org.simplemodeling.model.statemachine.ActivationStatus.Deactivated)
    }

    "clear suspension fields when status is restored" in {
      given ExecutionContext = ExecutionContext.test(SecurityContext.Privilege.ApplicationContentManager)
      val changes = ComponentFactory.statusUpdateFields(UserAccountStatus.Registered)
      changes.status shouldBe Update.set(UserAccountStatus.Registered)
      changes.suspendedAt shouldBe Update.setNull
      changes.suspendedBy shouldBe Update.setNull
      changes.suspensionReason shouldBe Update.setNull
      changes.resourceAttributes.activationStatus shouldBe Update.set(org.simplemodeling.model.statemachine.ActivationStatus.Active)
    }
  }

  private def _request(
    service: String,
    operation: String,
    properties: (String, Any)*
  ): Request =
    Request.ofService(
      service,
      operation,
      properties = properties.toList.map((k, v) => Property(k, v, None))
    )

  private def _execute(
    component: Component,
    ctx: ExecutionContext,
    action: org.goldenport.cncf.action.Action
  ): Consequence[OperationResponse] = {
    val call = component.logic.createActionCall(action, ctx)
    component.actionEngine.execute(call)
  }

  private def _component(): Component = {
    val subsystem = Subsystem(
      name = "textus-user-account-test",
      configuration = ResolvedConfiguration(Configuration.empty, ConfigurationTrace.empty),
      aliasResolver = AliasResolver.empty,
      runMode = RunMode.Command
    )
    val params = ComponentCreate(
      subsystem = subsystem,
      origin = ComponentOrigin.Builtin
    )
    val component = GeneratedDomainComponentLoader.create(params).head
    subsystem.add(Vector(component))
    subsystem.components.find(_.name == component.name).getOrElse(component)
  }

  private def _runtime_fixture(): _RuntimeFixture =
    _RuntimeFixture(ExecutionContext.create().runtime.core)

  private def _execution_context(
    fixture: _RuntimeFixture,
    security: SecurityContext
  ): ExecutionContext = {
    val base = ExecutionContext.create()
    val eventEngine = EventEngine.noop(DataStore.noop())
    lazy val context: ExecutionContext = _with_security(ExecutionContext.withRuntimeContext(base, runtime), security)
    lazy val uow: UnitOfWork = new UnitOfWork(context, eventEngine, CommitRecorder.noop)
    lazy val interpreter: UnitOfWorkInterpreter = new UnitOfWorkInterpreter(uow)
    lazy val idInterpreter = new (UnitOfWorkOp ~> Consequence) {
      def apply[A](fa: UnitOfWorkOp[A]): Consequence[A] =
        Consequence(interpreter.execute(fa))
    }
    lazy val runtime: RuntimeContext = new RuntimeContext(
      core = fixture.core,
      unitOfWorkSupplier = () => uow,
      unitOfWorkInterpreterFn = idInterpreter,
      commitAction = uowArg => {
        val _ = uowArg.commit()
        ()
      },
      abortAction = uowArg => {
        val _ = uowArg.rollback()
        ()
      },
      disposeAction = _ => (),
      token = "textus-user-account-action-runtime"
    )
    context
  }

  private def _with_security(
    ctx: ExecutionContext,
    security: SecurityContext
  ): ExecutionContext =
    ExecutionContext.withSecurityContext(ctx, security)

  private def _security_context(
    principalId: String,
    privilege: SecurityContext.Privilege,
    withAccessToken: Boolean = true,
    extraAttributes: Map[String, String] = Map.empty
  ): SecurityContext =
    SecurityContext(
      principal = new Principal {
        def id: PrincipalId = PrincipalId(principalId)
        def attributes: Map[String, String] =
          privilege.attributes ++
            (if (withAccessToken) Map("access_token" -> s"token-$principalId") else Map.empty) ++
            extraAttributes
      },
      capabilities = privilege.capabilities,
      level = privilege.level
    )

  private def _seed_user_account(
    id: EntityId,
    ownerPrincipalId: String,
    email: String
  )(using ExecutionContext): Consequence[Unit] = {
    val principal = ObjectId(Identifier(ownerPrincipalId))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = UserAccountCreateEntity(
      id = Some(id),
      nameAttributes = NameAttributes.simple(Name("owner")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      lifecycleAttributes = LifecycleAttributes(
        java.time.ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, java.time.ZoneOffset.UTC),
        None,
        Identifier("system"),
        None,
        org.simplemodeling.model.statemachine.PostStatus.default,
        org.simplemodeling.model.statemachine.Aliveness.default
      ),
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      email = email,
      emailVerifiedAt = None,
      phoneNumber = None,
      phoneVerifiedAt = None,
      lastLoginAt = None,
      passwordChangedAt = None,
      suspendedAt = None,
      suspendedBy = None,
      suspensionReason = None,
      status = UserAccountStatus.Registered
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _dummy_action(
    service: String,
    operation: String,
    properties: (String, Any)*
  ): Action =
    _DummyAction(_request(service, operation, properties*), operation)

  private final case class _RuntimeFixture(core: org.goldenport.cncf.context.ScopeContext.Core)

  private final case class _DummyAction(
    request: Request,
    operationName: String
  ) extends Action {
    override def name: String = operationName
    override def createCall(core: ActionCall.Core): ActionCall =
      _DummyActionCall(core, this)
  }

  private final case class _DummyActionCall(
    core: ActionCall.Core,
    override val action: _DummyAction
  ) extends ProcedureActionCall {
    override def execute(): Consequence[OperationResponse] =
      Consequence.success(OperationResponse.void)
  }
}
