package org.simplemodeling.textus.useraccount

import cats.~>
import cats.syntax.all.*
import org.goldenport.configuration.{Configuration, ConfigurationTrace, ResolvedConfiguration}
import org.goldenport.cncf.action.{Action, ActionCall, FunctionalActionCall, ProcedureActionCall}
import org.goldenport.cncf.component.{Component, ComponentCreate, ComponentOrigin}
import org.goldenport.cncf.context.{DataStoreContext, EntityStoreContext, ExecutionContext, Principal, PrincipalId, RuntimeContext, SecurityContext}
import org.goldenport.cncf.datastore.{DataStore, DataStoreSpace}
import org.goldenport.datatype.{Identifier, Name, ObjectId}
import org.goldenport.protocol.{Property, Request}
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.protocol.operation.OperationResponse.RecordResponse
import org.goldenport.record.Record
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.model.datatype.EntityId
import org.simplemodeling.model.value.{AuditAttributes, ContextualAttributes, DescriptiveAttributes, IdentityPresentation, LifecycleAttributes, MediaAttributes, NameAttributes, OrganizationSupport, PersonalProfile, PublicationAttributes, ResourceAttributes, SecurityAttributes}
import org.simplemodeling.textus.useraccount.entity.create.{AccessSession => AccessSessionCreateEntity}
import org.simplemodeling.textus.useraccount.entity.create.{Credential => CredentialCreateEntity}
import org.simplemodeling.textus.useraccount.entity.create.{UserProfile => UserProfileCreateEntity}
import org.simplemodeling.textus.useraccount.entity.create.{UserAccount => UserAccountCreateEntity}
import org.simplemodeling.textus.useraccount.entity.query.{AccessSession => AccessSessionQuery, RefreshSession => RefreshSessionQuery, UserAccount => UserAccountQuery, UserProfile => UserProfileQuery}
import org.simplemodeling.textus.useraccount.entity.update.{UserAccount => UserAccountUpdateEntity}
import org.simplemodeling.textus.useraccount.entity.create.UserAccount.given
import org.goldenport.cncf.entity.EntityStore
import java.net.URI
import java.security.MessageDigest

/*
 * @since   Apr.  7, 2026
 * @version Apr.  9, 2026
 * @author  ASAMI, Tomoharu
 */
final class UserAccountDataStoreUpdateSpec extends AnyWordSpec with Matchers {
  "test-owned datastore" should {
    "persist direct updates for a seeded user account" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val userId = _user_account_id("probe")
      _seed_user_account(userId, "probe_principal", "probe@example.com").toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userId).toOption.get
      val eid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userId).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get

      datastore.update(
        cid,
        eid,
        Record.dataAuto(
          "email_verified_at" -> "2026-04-07T00:00:00Z",
          "phone_number" -> "09012345678"
        )
      ).toOption.isDefined shouldBe true

      val stored = datastore.load(cid, eid).toOption.flatten
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getString("email_verified_at")) shouldBe Some("2026-04-07T00:00:00Z")
        stored.flatMap(_.getString("phone_number")) shouldBe Some("09012345678")
      }
    }

    "persist direct datastore updates executed through ActionEngine" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val component = _component()
      val userId = _user_account_id("probe_action")
      _seed_user_account(userId, "probe_principal", "probe-action@example.com").toOption.isDefined shouldBe true

      val action = _ProbeUpdateAction(userId)
      val call = component.logic.createActionCall(action, fixture.executionContext)
      component.actionEngine.execute(call).toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userId).toOption.get
      val eid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userId).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, eid).toOption.flatten
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getString("email_verified_at")) shouldBe Some("2026-04-07T00:00:00Z")
      }
    }

    "persist typed patch updates through entity_update action execution" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userId = _user_account_id("typed_update")
      _seed_user_account(userId, "probe_principal", "typed-update@example.com").toOption.isDefined shouldBe true

      val action = _ProbeEntityPatchAction(userId)
      val managerCtx = _with_security(
        fixture,
        "probe_principal",
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      val call = action.createCall(ActionCall.Core(action, managerCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userId).toOption.get
      val dsid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userId).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, dsid).toOption.flatten
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getAny("email_verified_at")) should not be empty
      }
    }

    "execute login with a manually built LoginCommand" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_manual")
      _seed_user_account(userId, ownerPrincipalId, "integration-manual@example.com").toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val request = Request.ofService(
        "User",
        "login",
        properties = List(
          Property("email", "integration-manual@example.com", None),
          Property("password", "secret", None)
        )
      )
      val action = org.simplemodeling.textus.useraccount.UserAccountComponent.UserService.LoginCommand(
        request,
        request.toRecord
      )
      val call = component.logic.createActionCall(action, anonymousCtx)
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
      val sessions = {
        val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
        summon[ExecutionContext].dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      }
      sessions.count(_.getString("user_account_id").contains(userId.print)) shouldBe 1
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("accessSessionId") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("refreshSessionId") should not be empty
    }

    "execute login after side-effect update wiring" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_only")
      _seed_user_account(
        userId,
        ownerPrincipalId,
        "integration@example.com",
        phoneNumber = Some("09012345678")
      ).toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val result = _execute_request(
        component,
        anonymousCtx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("email", "integration@example.com", None),
            Property("password", "secret", None)
          )
        )
      )
      result.toOption.isDefined shouldBe true
    }

    "resolve current user id from persisted access session token" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "session_owner"
      val userId = _user_account_id("session_current")
      val token = "token-session_owner"
      _seed_user_account(userId, ownerPrincipalId, "session-current@example.com").toOption.isDefined shouldBe true
      _seed_access_session(userId, ownerPrincipalId, token).toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(
        fixture,
        ownerPrincipalId,
        extraAttributes = Map("access_token" -> token)
      )
      ComponentFactory.currentLoggedInUserId()(using authenticatedCtx).toOption shouldBe Some(userId)
    }

    "load user account aggregate with its user profile" in {
      val component = _component()
      val ownerPrincipalId = "aggregate_owner"
      val ownerCtx = ExecutionContext.withSecurityContext(component.logic.executionContext(), _security_context(ownerPrincipalId))
      given ExecutionContext = ownerCtx
      val userId = _user_account_id("aggregate_profile")
      _seed_user_account(userId, ownerPrincipalId, "aggregate-profile@example.com").toOption.isDefined shouldBe true
      _seed_user_profile(userId, ownerPrincipalId).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        ownerCtx,
        Request.ofService(
          "aggregate",
          "loadUserAccount",
          properties = List(Property("id", userId.print, None))
        )
      )
      result.toOption.isDefined shouldBe true
    }

    "load user profile view" in {
      val component = _component()
      val ownerPrincipalId = "view_owner"
      val managerCtx = ExecutionContext.withSecurityContext(
        component.logic.executionContext(),
        _security_context("manager_principal", privilege = SecurityContext.Privilege.ApplicationContentManager)
      )
      given ExecutionContext = managerCtx
      val userId = _user_account_id("view_profile")
      val profileId = _user_profile_id("view_profile")
      _seed_user_account(userId, ownerPrincipalId, "view-profile@example.com").toOption.isDefined shouldBe true
      _seed_user_profile(userId, ownerPrincipalId, Some(profileId)).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        managerCtx,
        Request.ofService(
          "view",
          "loadUserProfile",
          properties = List(Property("id", profileId.print, None))
        )
      )
      result.toOption.isDefined shouldBe true
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      val identityPresentation = response.getRecord("identity_presentation").getOrElse(fail("identity_presentation missing"))
      identityPresentation.getString("display_name") shouldBe Some("Integration User")
      identityPresentation.getString("nickname") shouldBe Some("integ-user")
      identityPresentation.getString("avatar_url") shouldBe Some("https://example.com/avatar/integration-user.png")
    }

    "logout should revoke only the current session pair" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "logout_current_owner"
      val userId = _user_account_id("logout_current")
      val currentAccessToken = "access-token-current"
      val currentRefreshToken = "refresh-token-current"
      val otherAccessToken = "access-token-other"
      val otherRefreshToken = "refresh-token-other"
      _seed_user_account(userId, ownerPrincipalId, "logout-current@example.com").toOption.isDefined shouldBe true
      val currentRefreshId = _seed_refresh_session(userId, ownerPrincipalId, currentRefreshToken).toOption.get
      _seed_access_session(userId, ownerPrincipalId, currentAccessToken, Some(currentRefreshId)).toOption.isDefined shouldBe true
      _seed_refresh_session(userId, ownerPrincipalId, otherRefreshToken).toOption.isDefined shouldBe true
      _seed_access_session(userId, ownerPrincipalId, otherAccessToken, None).toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(
        fixture,
        ownerPrincipalId,
        extraAttributes = Map(
          "access_token" -> currentAccessToken,
          "refresh_token" -> currentRefreshToken
        )
      )
      val result = _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "logout",
          properties = List(Property("userAccountId", userId.print, None))
        )
      )
      result.toOption.isDefined shouldBe true

      val refreshCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshRecords = summon[ExecutionContext].dataStoreSpace.search(refreshCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val revokedRefreshCount = refreshRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").nonEmpty)
      revokedRefreshCount shouldBe 1
      val activeRefreshCount = refreshRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeRefreshCount shouldBe 1

      val accessCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessRecords = summon[ExecutionContext].dataStoreSpace.search(accessCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val revokedAccessCount = accessRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").nonEmpty)
      revokedAccessCount shouldBe 1
      val activeAccessCount = accessRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeAccessCount shouldBe 1
    }

    "logoutAll should revoke all session pairs for the account" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "logout_all_owner"
      val userId = _user_account_id("logout_all")
      val currentAccessToken = "access-token-all-current"
      val currentRefreshToken = "refresh-token-all-current"
      _seed_user_account(userId, ownerPrincipalId, "logout-all@example.com").toOption.isDefined shouldBe true
      val refreshId1 = _seed_refresh_session(userId, ownerPrincipalId, currentRefreshToken).toOption.get
      _seed_access_session(userId, ownerPrincipalId, currentAccessToken, Some(refreshId1)).toOption.isDefined shouldBe true
      _seed_refresh_session(userId, ownerPrincipalId, "refresh-token-all-other").toOption.isDefined shouldBe true
      _seed_access_session(userId, ownerPrincipalId, "access-token-all-other", None).toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(
        fixture,
        ownerPrincipalId,
        extraAttributes = Map(
          "access_token" -> currentAccessToken,
          "refresh_token" -> currentRefreshToken
        )
      )
      val result = _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "logoutAll",
          properties = List(Property("userAccountId", userId.print, None))
        )
      )
      result.toOption.isDefined shouldBe true

      val refreshCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshRecords = summon[ExecutionContext].dataStoreSpace.search(refreshCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val activeRefreshCount = refreshRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeRefreshCount shouldBe 0

      val accessCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessRecords = summon[ExecutionContext].dataStoreSpace.search(accessCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val activeAccessCount = accessRecords.count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeAccessCount shouldBe 0
    }

    "rotate refresh token and issue a new token pair" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "refresh_owner"
      val userId = _user_account_id("refresh_pair")
      val refreshToken = "refresh-token-owner"
      _seed_user_account(userId, ownerPrincipalId, "refresh@example.com").toOption.isDefined shouldBe true
      val originalRefreshId = _seed_refresh_session(userId, ownerPrincipalId, refreshToken).toOption.get

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val result = _execute_request(
        component,
        anonymousCtx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshToken, None))
        )
      )
      result.toOption.isDefined shouldBe true
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("refreshToken") should not be Some(refreshToken)

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val records = summon[ExecutionContext].dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val previous = records.find(_.getString("id").contains(originalRefreshId.print)).get
      previous.getString("rotated_at") should not be empty
      previous.getString("successor_session_id") should not be empty
      records.count(_.getString("user_account_id").contains(userId.print)) shouldBe 2
    }

    "detect refresh token reuse and revoke remaining sessions" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "refresh_reuse_owner"
      val userId = _user_account_id("refresh_reuse")
      val refreshToken = "refresh-token-reuse"
      _seed_user_account(userId, ownerPrincipalId, "reuse@example.com").toOption.isDefined shouldBe true
      _seed_refresh_session(userId, ownerPrincipalId, refreshToken).toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val first = _execute_request(
        component,
        anonymousCtx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshToken, None))
        )
      )
      first.toOption.isDefined shouldBe true

      val second = _execute_request(
        component,
        anonymousCtx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshToken, None))
        )
      )
      second.toOption.isDefined shouldBe false

      val refreshCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshRecords = summon[ExecutionContext].dataStoreSpace.search(refreshCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val activeRefreshCount = refreshRecords
        .count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeRefreshCount shouldBe 0

      val accessCid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessRecords = summon[ExecutionContext].dataStoreSpace.search(accessCid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector
      val activeAccessCount = accessRecords
        .count(r => r.getString("user_account_id").contains(userId.print) && r.getString("revoked_at").isEmpty)
      activeAccessCount shouldBe 0
    }

    "surface current login-like patch failure under anonymous context" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_patch")
      _seed_user_account(userId, ownerPrincipalId, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeEntityPatchAction(userId)
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe false
      result.toString should include("Permission")
    }

    "persist current login-like typed load and working-set side effect path" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_workingset")
      _seed_user_account(userId, ownerPrincipalId, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeWorkingSetUpsertAction(userId)
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist login-like working-set put plus login-shaped response" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_shape")
      _seed_user_account(userId, ownerPrincipalId, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeLoginWorkingSetOnlyAction(userId)
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist combined login side effects outside the real login operation path" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_side_effects")
      _seed_user_account(userId, ownerPrincipalId, "integration@example.com").toOption.isDefined shouldBe true

      val managerCtx = _with_security(
        fixture,
        ownerPrincipalId,
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      val action = _ProbeLoginSideEffectsAction(userId)
      val call = action.createCall(ActionCall.Core(action, managerCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "keep working when only raw user lookup is included in a procedure probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_user_proc")
      _seed_user_account(userId, ownerPrincipalId, "integration-user-proc@example.com").toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawUserLookupProcedureAction("integration-user-proc@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "keep working when only raw user lookup is included in a probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_user_only")
      _seed_user_account(userId, ownerPrincipalId, "integration-user-only@example.com").toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawUserLookupLoginAction("integration-user-only@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "show recovered entity id from raw user lookup record" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_id_compare")
      _seed_user_account(userId, ownerPrincipalId, "integration-id-compare@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeCompareRecoveredUserIdAction(userId, "integration-id-compare@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "show raw user-account search records before typed lookup" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_dump")
      _seed_user_account(userId, ownerPrincipalId, "integration-dump@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeDumpRawUserRecordsAction("integration-dump@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "surface current duplicate failure when only typed user load is included in a probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_typed_load")
      _seed_user_account(userId, ownerPrincipalId, "integration-typed-load@example.com").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeTypedUserLoadAction(userId)
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "surface entity update authorization failure in a raw lookup probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("login_probe_raw")
      _seed_user_account(userId, ownerPrincipalId, "integration-raw@example.com").toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val anonymousCtx = _with_security(
        fixture,
        ownerPrincipalId,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawLookupLoginAction("integration-raw@example.com", "secret")
      val call = action.createCall(ActionCall.Core(action, anonymousCtx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe false
      result.toString should include("Permission")
    }

    "persist verification updates through actual component operations" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerPrincipalId = "integration_owner"
      val userId = _user_account_id("integration")
      _seed_user_account(
        userId,
        ownerPrincipalId,
        "integration@example.com",
        phoneNumber = Some("09012345678")
      ).toOption.isDefined shouldBe true
      _seed_credential(userId, ownerPrincipalId, "secret").toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(fixture, userId.print)
      ComponentFactory.addLoggedInUserForTest(userId)(using authenticatedCtx).toOption.isDefined shouldBe true
      _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "verifyMyEmail",
          properties = List(
            Property("proofToken", "email-proof", None)
          )
        )
      ).toOption.isDefined shouldBe true

      _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "verifyMyPhone",
          properties = List(
            Property("phoneNumber", "09012345678", None),
            Property("proofToken", "phone-proof", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val cid = fixture.executionContext.entityStoreSpace.dataStoreCollection(userId).toOption.get
      val dsid = fixture.executionContext.entityStoreSpace.dataStoreEntryId(userId).toOption.get
      val datastore = fixture.executionContext.dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, dsid).toOption.flatten
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getAny("email_verified_at")) should not be empty
        stored.flatMap(_.getAny("phone_verified_at")) should not be empty
        stored.flatMap(_.getString("phone_number")) shouldBe Some("09012345678")
        stored.flatMap(_.getAny("last_login_at")) shouldBe empty
      }
    }

    "reject email verification when the current account email is already verified" in {
      val fixture = _fixture()
      val component = _component()
      val userId = _user_account_id("already_verified_email")
      val ownerPrincipalId = "already_verified_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerPrincipalId))
      _seed_user_account(
        userId,
        ownerPrincipalId,
        "already-verified-email@example.com",
        emailVerifiedAt = Some("2026-04-08T00:00:00Z")
      ).toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(fixture, userId.print)
      ComponentFactory.addLoggedInUserForTest(userId)(using authenticatedCtx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "verifyMyEmail",
          properties = List(
            Property("proofToken", "email-proof", None)
          )
        )
      )
      result.toOption shouldBe empty
      result.toString.toLowerCase(java.util.Locale.ROOT) should include("already verified")
    }

    "reject phone verification when the current account does not have a stored phone number" in {
      val fixture = _fixture()
      val component = _component()
      val userId = _user_account_id("missing_phone")
      val ownerPrincipalId = "missing_phone_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerPrincipalId))
      _seed_user_account(userId, ownerPrincipalId, "missing-phone@example.com").toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(fixture, userId.print)
      ComponentFactory.addLoggedInUserForTest(userId)(using authenticatedCtx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "verifyMyPhone",
          properties = List(
            Property("phoneNumber", "09012345678", None),
            Property("proofToken", "phone-proof", None)
          )
        )
      )
      result.toOption shouldBe empty
      result.toString.toLowerCase(java.util.Locale.ROOT) should include("does not have an sms contact")
    }

    "reject phone verification when the requested phone number does not match the current account" in {
      val fixture = _fixture()
      val component = _component()
      val userId = _user_account_id("phone_mismatch")
      val ownerPrincipalId = "phone_mismatch_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerPrincipalId))
      _seed_user_account(
        userId,
        ownerPrincipalId,
        "phone-mismatch@example.com",
        phoneNumber = Some("09000000000")
      ).toOption.isDefined shouldBe true

      val authenticatedCtx = _with_security(fixture, userId.print)
      ComponentFactory.addLoggedInUserForTest(userId)(using authenticatedCtx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedCtx,
        Request.ofService(
          "User",
          "verifyMyPhone",
          properties = List(
            Property("phoneNumber", "09012345678", None),
            Property("proofToken", "phone-proof", None)
          )
        )
      )
      result.toOption shouldBe empty
      result.toString.toLowerCase(java.util.Locale.ROOT) should include("does not match")
    }

    "persist suspension details through actual status update operations" in {
      val fixture = _fixture()
      val component = _component()
      val managerCtx = _with_security(
        fixture,
        SecurityContext.Privilege.ApplicationContentManager.principalId.value,
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      given ExecutionContext = managerCtx
      val userId = _user_account_id("suspend_restore")
      _seed_user_account(userId, "integration_owner", "suspend-restore@example.com").toOption.isDefined shouldBe true

      _execute_request(
        component,
        managerCtx,
        Request.ofService(
          "Management",
          "updateUserStatus",
          properties = List(
            Property("userAccountId", userId.print, None),
            Property("status", "suspended", None),
            Property("suspensionReason", "policy_violation", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userId).toOption.get
      val dsid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userId).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val suspended = datastore.load(cid, dsid).toOption.flatten
      val suspendedSummary = suspended.map(_.fields.map(f => f.key -> Option(f.value).map(_.toString).orNull).toVector)
      withClue(s"suspended=${suspendedSummary}") {
        suspended.flatMap(_.getAny("status")).map(_.toString.contains("suspended")) shouldBe Some(true)
        suspended.flatMap(_.getAny("suspended_at")) should not be empty
        suspended.flatMap(_.getString("suspended_by")) shouldBe Some(SecurityContext.Privilege.ApplicationContentManager.principalId.value)
        suspended.flatMap(_.getString("suspension_reason")) shouldBe Some("policy_violation")
      }

      _execute_request(
        component,
        managerCtx,
        Request.ofService(
          "Management",
          "updateUserStatus",
          properties = List(
            Property("userAccountId", userId.print, None),
            Property("status", "registered", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val restored = datastore.load(cid, dsid).toOption.flatten
      val restoredSummary = restored.map(r =>
        Vector(
          "status" -> r.getAny("status").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspended_at" -> r.getAny("suspended_at").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspended_by" -> r.getAny("suspended_by").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspension_reason" -> r.getAny("suspension_reason").flatMap(x => Option(x).map(_.toString)).orNull
        )
      )
      withClue(s"restored=${restoredSummary}") {
        restored.flatMap(_.getAny("status")).map(_.toString.contains("registered")) shouldBe Some(true)
        restored.flatMap(_.getAny("suspended_at")).forall(java.util.Objects.isNull) shouldBe true
        restored.flatMap(_.getAny("suspended_by")).forall(java.util.Objects.isNull) shouldBe true
        restored.flatMap(_.getAny("suspension_reason")).forall(java.util.Objects.isNull) shouldBe true
      }
    }
  }

  private def _fixture(): _Fixture = {
    ComponentFactory.resetLoginWorkingSetForTest()
    val datastoreSpace = new DataStoreSpace().addDataStore(DataStore.inMemorySearchable())
    val entityStoreSpace = org.goldenport.cncf.entity.EntityStoreSpace.create(
      ResolvedConfiguration(Configuration.empty, ConfigurationTrace.empty)
    )
    val base = ExecutionContext.create()
    val core = RuntimeContext.core(
      name = "textus-user-account-datastore-probe",
      parent = None,
      observabilityContext = base.observability,
      datastore = Some(DataStoreContext(datastoreSpace)),
      entitystore = Some(EntityStoreContext(entityStoreSpace))
    )
    val eventEngine = org.goldenport.cncf.event.EventEngine.noop(DataStore.noop())
    def build(security: SecurityContext): ExecutionContext = {
      lazy val context: ExecutionContext = ExecutionContext.withSecurityContext(
        ExecutionContext.withRuntimeContext(base, runtime),
        security
      )
      lazy val uow = new org.goldenport.cncf.unitofwork.UnitOfWork(context, eventEngine, org.goldenport.cncf.unitofwork.CommitRecorder.noop)
      lazy val interpreter = new org.goldenport.cncf.unitofwork.UnitOfWorkInterpreter(uow)
      lazy val idInterpreter = new (org.goldenport.cncf.unitofwork.UnitOfWorkOp ~> org.goldenport.Consequence) {
        def apply[A](fa: org.goldenport.cncf.unitofwork.UnitOfWorkOp[A]) = org.goldenport.Consequence(interpreter.execute(fa))
      }
      lazy val runtime = new RuntimeContext(
        core = core,
        unitOfWorkSupplier = () => uow,
        unitOfWorkInterpreterFn = idInterpreter,
        commitAction = uowArg => { val _ = uowArg.commit(); () },
        abortAction = uowArg => { val _ = uowArg.rollback(); () },
        disposeAction = _ => (),
        token = "textus-user-account-datastore-probe"
      )
      context
    }
    _Fixture(build(_security_context("probe_principal")), build)
  }

  private def _execute_request(
    component: Component,
    ctx: ExecutionContext,
    request: Request
  ): org.goldenport.Consequence[OperationResponse] =
    component.logic.makeOperationRequest(request).flatMap {
      case action: Action =>
        val call = component.logic.createActionCall(action, ctx)
        component.actionEngine.execute(call)
      case other =>
        org.goldenport.Consequence.failure(s"request did not resolve to action: $other")
    }

  private def _with_security(
    fixture: _Fixture,
    principalId: String,
    withAccessToken: Boolean = true,
    extraAttributes: Map[String, String] = Map.empty,
    privilege: SecurityContext.Privilege = SecurityContext.Privilege.User
  ): ExecutionContext =
    fixture.contextFor(_security_context(principalId, withAccessToken, extraAttributes, privilege))

  private def _security_context(
    principalId: String,
    withAccessToken: Boolean = true,
    extraAttributes: Map[String, String] = Map.empty,
    privilege: SecurityContext.Privilege = SecurityContext.Privilege.User
  ): SecurityContext = {
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
  }

  private def _seed_user_account(
    id: EntityId,
    ownerPrincipalId: String,
    email: String,
    emailVerifiedAt: Option[String] = None,
    phoneNumber: Option[String] = None,
    phoneVerifiedAt: Option[String] = None
  )(using ExecutionContext) = {
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
      loginName = None,
      externalSubjectId = None,
      emailVerifiedAt = emailVerifiedAt,
      phoneNumber = phoneNumber,
      phoneVerifiedAt = phoneVerifiedAt,
      lastLoginAt = None,
      passwordChangedAt = None,
      suspendedAt = None,
      suspendedBy = None,
      suspensionReason = None,
      status = UserAccountStatus.Registered
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _seed_credential(
    userId: EntityId,
    ownerPrincipalId: String,
    password: String
  )(using ExecutionContext) = {
    val credentialId = _credential_id("credential")
    val principal = ObjectId(Identifier(ownerPrincipalId))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = CredentialCreateEntity(
      id = Some(credentialId),
      nameAttributes = NameAttributes.simple(Name("credential")),
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
      userAccountId = Some(userId),
      passwordHash = _password_hash(password)
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _seed_access_session(
    userId: EntityId,
    ownerPrincipalId: String,
    token: String,
    refreshSessionId: Option[EntityId] = None
  )(using ExecutionContext) = {
    val sessionId = _access_session_id("access_session")
    val principal = ObjectId(Identifier(ownerPrincipalId))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = AccessSessionCreateEntity(
      id = Some(sessionId),
      nameAttributes = NameAttributes.simple(Name("access_session")),
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
      userAccountId = Some(userId),
      refreshSessionId = refreshSessionId.map(_.print),
      tokenHash = _password_hash(token),
      issuedAt = "2026-04-09T00:00:00Z",
      expiresAt = "2099-01-01T00:00:00Z",
      revokedAt = None,
      lastAccessedAt = None,
      clientId = None,
      deviceInfo = None,
      ipAddress = None,
      userAgent = None
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _seed_user_profile(
    userId: EntityId,
    ownerPrincipalId: String,
    profileId: Option[EntityId] = None
  )(using ExecutionContext) = {
    val id = profileId.getOrElse(_user_profile_id("user_profile"))
    val principal = ObjectId(Identifier(ownerPrincipalId))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = UserProfileCreateEntity(
      id = Some(id),
      nameAttributes = NameAttributes.simple(Name("user_profile")),
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
      userAccountId = Some(userId),
      identityPresentation = Some(
        IdentityPresentation.create(
          Name("Integration"),
          Name("User"),
          Name("インテグレーション"),
          Name("ユーザー"),
          Name("Integration User"),
          Name("integ-user"),
          URI.create("https://example.com/avatar/integration-user.png").toURL
        )
      ),
      personalProfile = None,
      organizationSupport = Some(
        OrganizationSupport.create(
          Name("Textus"),
          Name("Engineering"),
          Name("Principal User")
        )
      )
    )
    EntityStore.standard().create(entity).map(_ => id)
  }

  private def _seed_refresh_session(
    userId: EntityId,
    ownerPrincipalId: String,
    token: String
  )(using ExecutionContext) = {
    val sessionId = _refresh_session_id("refresh_session")
    val principal = ObjectId(Identifier(ownerPrincipalId))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = org.simplemodeling.textus.useraccount.entity.create.RefreshSession(
      id = Some(sessionId),
      nameAttributes = NameAttributes.simple(Name("refresh_session")),
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
      userAccountId = Some(userId),
      successorSessionId = None,
      tokenHash = _password_hash(token),
      issuedAt = "2026-04-09T00:00:00Z",
      expiresAt = "2099-01-01T00:00:00Z",
      revokedAt = None,
      rotatedAt = None,
      clientId = None,
      deviceInfo = None,
      ipAddress = None,
      userAgent = None
    )
    EntityStore.standard().create(entity).map(_ => sessionId)
  }

  private def _user_account_id(label: String): EntityId =
    _entity_id(UserAccountQuery.collectionId, label)

  private def _credential_id(label: String): EntityId =
    _entity_id(org.simplemodeling.textus.useraccount.entity.query.Credential.collectionId, label)

  private def _access_session_id(label: String): EntityId =
    _entity_id(AccessSessionQuery.collectionId, label)

  private def _refresh_session_id(label: String): EntityId =
    _entity_id(RefreshSessionQuery.collectionId, label)

  private def _user_profile_id(label: String): EntityId =
    _entity_id(UserProfileQuery.collectionId, label)

  private def _entity_id(
    collectionId: org.simplemodeling.model.datatype.EntityCollectionId,
    label: String
  ): EntityId =
    EntityId(
      collectionId.major,
      collectionId.minor,
      collectionId,
      entropy = Some(_id_entropy(label))
    )

  private def _id_entropy(label: String): String = {
    val normalized = label.replaceAll("[^A-Za-z0-9_]", "_")
    val head = if normalized.headOption.exists(_.isLetter) then normalized else s"x_$normalized"
    s"${head}_${System.nanoTime}"
  }

  private def _password_hash(password: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(password.getBytes("UTF-8")).iterator.map(b => f"${b & 0xff}%02x").mkString
  }

  private final case class _Fixture(
    executionContext: ExecutionContext,
    contextFor: SecurityContext => ExecutionContext
  )

  private def _component(): Component = {
    val subsystem = org.goldenport.cncf.subsystem.Subsystem(
      name = "textus-user-account-datastore-probe",
      configuration = ResolvedConfiguration(Configuration.empty, ConfigurationTrace.empty),
      aliasResolver = org.goldenport.cncf.path.AliasResolver.empty,
      runMode = org.goldenport.cncf.cli.RunMode.Command
    )
    val params = ComponentCreate(
      subsystem = subsystem,
      origin = ComponentOrigin.Builtin
    )
    val component = GeneratedDomainComponentLoader.create(params).head
    subsystem.add(Vector(component))
    subsystem.components.find(_.name == component.name).getOrElse(component)
  }

  private final case class _ProbeUpdateAction(userId: EntityId) extends Action {
    override def name: String = "probeDirectUpdate"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeDirectUpdate",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeUpdateActionCall(core, this)
  }

  private final case class _ProbeEntityPatchAction(userId: EntityId) extends Action {
    override def name: String = "probeEntityPatch"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeEntityPatch",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeEntityPatchActionCall(core, this)
  }

  private final case class _ProbeWorkingSetUpsertAction(userId: EntityId) extends Action {
    override def name: String = "probeWorkingSetUpsert"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeWorkingSetUpsert",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeWorkingSetUpsertActionCall(core, this)
  }

  private final case class _ProbeLoginWorkingSetOnlyAction(
    userId: EntityId
  ) extends Action {
    override def name: String = "probeLoginWorkingSetOnly"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeLoginWorkingSetOnly",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeLoginWorkingSetOnlyActionCall(core, this)
  }

  private final case class _ProbeLoginSideEffectsAction(
    userId: EntityId
  ) extends Action {
    override def name: String = "probeLoginSideEffects"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeLoginSideEffects",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeLoginSideEffectsActionCall(core, this)
  }

  private final case class _ProbeCompareRecoveredUserIdAction(
    expectedUserId: EntityId,
    email: String
  ) extends Action {
    override def name: String = "probeCompareRecoveredUserId"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeCompareRecoveredUserId",
        properties = List(
          Property("userAccountId", expectedUserId.print, None),
          Property("email", email, None)
        )
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeCompareRecoveredUserIdActionCall(core, this)
  }

  private final case class _ProbeDumpRawUserRecordsAction(
    email: String
  ) extends Action {
    override def name: String = "probeDumpRawUserRecords"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeDumpRawUserRecords",
        properties = List(Property("email", email, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeDumpRawUserRecordsActionCall(core, this)
  }

  private final case class _ProbeTypedUserLoadAction(
    userId: EntityId
  ) extends Action {
    override def name: String = "probeTypedUserLoad"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeTypedUserLoad",
        properties = List(Property("userAccountId", userId.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeTypedUserLoadActionCall(core, this)
  }

  private final case class _ProbeRawUserLookupProcedureAction(
    email: String
  ) extends Action {
    override def name: String = "probeRawUserLookupProcedure"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeRawUserLookupProcedure",
        properties = List(Property("email", email, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeRawUserLookupProcedureActionCall(core, this)
  }

  private final case class _ProbeRawUserLookupLoginAction(
    email: String
  ) extends Action {
    override def name: String = "probeRawUserLookupLogin"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeRawUserLookupLogin",
        properties = List(
          Property("email", email, None)
        )
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeRawUserLookupLoginActionCall(core, this)
  }

  private final case class _ProbeRawLookupLoginAction(
    email: String,
    password: String
  ) extends Action {
    override def name: String = "probeRawLookupLogin"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeRawLookupLogin",
        properties = List(
          Property("email", email, None),
          Property("password", password, None)
        )
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeRawLookupLoginActionCall(core, this)
  }

  private final case class _ProbeUpdateActionCall(
    core: ActionCall.Core,
    override val action: _ProbeUpdateAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val cid = executionContext.entityStoreSpace.dataStoreCollection(action.userId).TAKE
      val eid = executionContext.entityStoreSpace.dataStoreEntryId(action.userId).TAKE
      val datastore = executionContext.dataStoreSpace.dataStore(cid).TAKE
      datastore.update(
        cid,
        eid,
        Record.dataAuto("email_verified_at" -> "2026-04-07T00:00:00Z")
      ).TAKE
      org.goldenport.Consequence.success(OperationResponse.void)
    }
  }

  private final case class _ProbeEntityPatchActionCall(
    core: ActionCall.Core,
    override val action: _ProbeEntityPatchAction
  ) extends FunctionalActionCall
      with ActionCall.Core.Holder
      with org.goldenport.cncf.action.ActionCallFeaturePart {
    protected def build_Program =
      for
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("email_verified_at" -> "2026-04-07T00:00:00Z")))
        _ <- entity_update(action.userId, patch)
      yield
        OperationResponse.void
  }

  private final case class _ProbeWorkingSetUpsertActionCall(
    core: ActionCall.Core,
    override val action: _ProbeWorkingSetUpsertAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val user =
        EntityStore
          .standard()
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userId)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userId))
          .TAKE
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.put(user))
      org.goldenport.Consequence.success(OperationResponse.void)
    }
  }

  private final case class _ProbeLoginWorkingSetOnlyActionCall(
    core: ActionCall.Core,
    override val action: _ProbeLoginWorkingSetOnlyAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val user =
        EntityStore
          .standard()
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userId)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userId))
          .TAKE
      component
        .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
        .flatMap(_.storage.memoryRealm)
        .foreach(_.put(user))
      org.goldenport.Consequence.success(
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "authenticated" -> true
          )
        )
      )
    }
  }

  private final case class _ProbeLoginSideEffectsActionCall(
    core: ActionCall.Core,
    override val action: _ProbeLoginSideEffectsAction
  ) extends FunctionalActionCall
      with ActionCall.Core.Holder
      with org.goldenport.cncf.action.ActionCallFeaturePart {
    protected def build_Program =
      for
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("last_login_at" -> "2026-04-07T00:00:00Z")))
        _ <- entity_update(action.userId, patch)
        user <- exec_from(
          {
            given ExecutionContext = executionContext
            EntityStore
              .standard()
              .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userId)
          }
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userId))
        )
        _ <- exec_from {
          component
            .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
            .flatMap(_.storage.memoryRealm)
            .foreach(_.put(user))
          org.goldenport.Consequence.unit
        }
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "authenticated" -> true
          )
        )
  }

  private final case class _ProbeCompareRecoveredUserIdActionCall(
    core: ActionCall.Core,
    override val action: _ProbeCompareRecoveredUserIdAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val cid = executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId).TAKE
      val result = executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).TAKE
      val record = result.records.toVector.find(_.getString("email").contains(action.email)).get
      val recovered = record.getAsC[EntityId]("id").flatMap(x => org.goldenport.Consequence.successOrPropertyNotFound("id", x)).TAKE
      println(s"EXPECTED-ID=${action.expectedUserId.print}")
      println(s"RECOVERED-ID=${recovered.print}")
      println(s"EXPECTED-COLLECTION=${action.expectedUserId.collection}")
      println(s"RECOVERED-COLLECTION=${recovered.collection}")
      org.goldenport.Consequence.success(
        OperationResponse.create(
          Record.data(
            "expected" -> action.expectedUserId.print,
            "recovered" -> recovered.print
          )
        )
      )
    }
  }

  private final case class _ProbeDumpRawUserRecordsActionCall(
    core: ActionCall.Core,
    override val action: _ProbeDumpRawUserRecordsAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val cid = executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId).TAKE
      val result = executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).TAKE
      val rows = result.records.toVector.map { r =>
        Record.dataAuto(
          "id" -> r.getString("id").getOrElse("<none>"),
          "email" -> r.getString("email").getOrElse("<none>"),
          "keys" -> r.fields.map(_.key).mkString(",")
        )
      }
      println("RAW-USER-RECORDS=" + rows.map(_.asMap).mkString(" | "))
      org.goldenport.Consequence.success(OperationResponse.create(Record.data("count" -> rows.size.toString)))
    }
  }

  private final case class _ProbeTypedUserLoadActionCall(
    core: ActionCall.Core,
    override val action: _ProbeTypedUserLoadAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val user =
        EntityStore
          .standard()
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userId)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userId))
          .TAKE
      org.goldenport.Consequence.success(
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "authenticated" -> true
          )
        )
      )
    }
  }

  private final case class _ProbeRawUserLookupProcedureActionCall(
    core: ActionCall.Core,
    override val action: _ProbeRawUserLookupProcedureAction
  ) extends ProcedureActionCall {
    override def execute(): org.goldenport.Consequence[OperationResponse] = {
      given ExecutionContext = executionContext
      val cid = executionContext.entityStoreSpace.dataStoreCollection(UserAccountQuery.collectionId).TAKE
      val result = executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).TAKE
      val ids = result.records.toVector
        .filter(_.getString("email").contains(action.email))
        .map(_.getAsC[EntityId]("id").flatMap(x => org.goldenport.Consequence.successOrPropertyNotFound("id", x)).TAKE)
      val user = EntityStore
        .standard()
        .load[org.simplemodeling.textus.useraccount.entity.UserAccount](ids.head)
        .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(ids.head))
        .TAKE
      org.goldenport.Consequence.success(
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "authenticated" -> true
          )
        )
      )
    }
  }

  private final case class _ProbeRawUserLookupLoginActionCall(
    core: ActionCall.Core,
    override val action: _ProbeRawUserLookupLoginAction
  ) extends FunctionalActionCall
      with ActionCall.Core.Holder
      with org.goldenport.cncf.action.ActionCallFeaturePart {
    protected def build_Program =
      for
        user <- exec_from(_raw_user_by_email(action.email)(using executionContext))
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "authenticated" -> true
          )
        )

    private def _raw_user_by_email(email: String)(using ExecutionContext): org.goldenport.Consequence[org.simplemodeling.textus.useraccount.entity.UserAccount] =
      for
        records <- _raw_records(UserAccountQuery.collectionId)
        ids <- records.filter(_.getString("email").contains(email)).traverse(_entity_id_of)
        users <- ids.traverse(id =>
          EntityStore
            .standard()
            .load[org.simplemodeling.textus.useraccount.entity.UserAccount](id)(using summon, executionContext)
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(id))
        )
        user <- users.headOption match
          case Some(u) => org.goldenport.Consequence.success(u)
          case None => org.goldenport.Consequence.failure(s"user account not found by email: $email")
      yield user

    private def _raw_records(collectionId: org.simplemodeling.model.datatype.EntityCollectionId)(using ExecutionContext): org.goldenport.Consequence[Vector[Record]] =
      for
        cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionId)
        result <- executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty))
      yield result.records.toVector

    private def _entity_id_of(record: Record): org.goldenport.Consequence[EntityId] =
      record.getAsC[EntityId]("id").flatMap(x => org.goldenport.Consequence.successOrPropertyNotFound("id", x))
  }

  private final case class _ProbeRawLookupLoginActionCall(
    core: ActionCall.Core,
    override val action: _ProbeRawLookupLoginAction
  ) extends FunctionalActionCall
      with ActionCall.Core.Holder
      with org.goldenport.cncf.action.ActionCallFeaturePart {
    protected def build_Program =
      for
        user <- exec_from(_raw_user_by_email(action.email)(using executionContext))
        credential <- exec_from(_raw_credential_by_user_and_password(user.id, _password_hash(action.password))(using executionContext))
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("last_login_at" -> "2026-04-07T00:00:00Z")))
        _ <- entity_update(user.id, patch)
        loaded <- exec_from(
          EntityStore
            .standard()
            .load[org.simplemodeling.textus.useraccount.entity.UserAccount](user.id)(using summon, executionContext)
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(user.id))
        )
        _ <- exec_from {
          component
            .flatMap(_.entitySpace.entityOption[Any](UserAccountQuery.collectionId.name))
            .flatMap(_.storage.memoryRealm)
            .foreach(_.put(loaded))
          org.goldenport.Consequence.unit
        }
      yield
        OperationResponse(
          Record.data(
            "userAccountId" -> user.id.print,
            "credentialId" -> credential.id.print,
            "authenticated" -> true
          )
        )

    private def _raw_user_by_email(email: String)(using ExecutionContext): org.goldenport.Consequence[org.simplemodeling.textus.useraccount.entity.UserAccount] =
      for
        records <- _raw_records(UserAccountQuery.collectionId)
        ids <- records
          .filter(_.getString("email").contains(email))
          .traverse(_entity_id_of)
        users <- ids.traverse(id =>
          EntityStore
            .standard()
            .load[org.simplemodeling.textus.useraccount.entity.UserAccount](id)(using summon, executionContext)
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(id))
        )
        user <- users.headOption match
          case Some(u) => org.goldenport.Consequence.success(u)
          case None => org.goldenport.Consequence.failure(s"user account not found by email: $email")
      yield user

    private def _raw_credential_by_user_and_password(
      userId: EntityId,
      hashedPassword: String
    )(using ExecutionContext): org.goldenport.Consequence[org.simplemodeling.textus.useraccount.entity.Credential] =
      for
        records <- _raw_records(org.simplemodeling.textus.useraccount.entity.query.Credential.collectionId)
        ids <- records
          .filter(r =>
            r.getString("user_account_id").contains(userId.print) &&
              r.getString("password_hash").contains(hashedPassword)
          )
          .traverse(_entity_id_of)
        credentials <- ids.traverse(id =>
          EntityStore
            .standard()
            .load[org.simplemodeling.textus.useraccount.entity.Credential](id)(using summon, executionContext)
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(id))
        )
        credential <- credentials.headOption match
          case Some(c) => org.goldenport.Consequence.success(c)
          case None => org.goldenport.Consequence.failure("invalid credentials")
      yield credential

    private def _raw_records(collectionId: org.simplemodeling.model.datatype.EntityCollectionId)(using ExecutionContext): org.goldenport.Consequence[Vector[Record]] =
      for
        cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionId)
        result <- executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty))
      yield result.records.toVector

    private def _entity_id_of(record: Record): org.goldenport.Consequence[EntityId] =
      record.getAsC[EntityId]("id").flatMap(x => org.goldenport.Consequence.successOrPropertyNotFound("id", x))
  }

}
