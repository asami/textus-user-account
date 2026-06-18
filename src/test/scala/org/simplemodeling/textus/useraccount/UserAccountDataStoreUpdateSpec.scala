package org.simplemodeling.textus.useraccount

import cats.~>
import cats.syntax.all.*
import org.goldenport.configuration.{Configuration, ConfigurationTrace, ConfigurationValue, ResolvedConfiguration}
import org.goldenport.cncf.action.{Action, ActionCall, FunctionalActionCall, ProcedureActionCall}
import org.goldenport.cncf.component.{Component, ComponentCreate, ComponentOrigin}
import org.goldenport.cncf.component.builtin.messagedeliverystub.MessageDeliveryStubComponent
import org.goldenport.cncf.context.{DataStoreContext, EntityStoreContext, ExecutionContext, Principal, PrincipalId, RuntimeContext, SecurityContext}
import org.goldenport.cncf.security.AuthenticationRequest
import org.goldenport.cncf.datastore.{DataStore, DataStoreSpace}
import org.goldenport.cncf.datastore.sql.SqlDataStore
import org.goldenport.datatype.{Identifier, Name, ObjectId}
import org.goldenport.protocol.{Property, Request}
import org.goldenport.protocol.operation.OperationResponse
import org.goldenport.protocol.operation.OperationResponse.RecordResponse
import org.goldenport.record.Record
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.simplemodeling.model.datatype.EntityId
import org.simplemodeling.model.value.{AuditAttributes, ContentAttributes, ContextualAttributes, DescriptiveAttributes, IdentityPresentation, LifecycleAttributes, MediaAttributes, NameAttributes, OrganizationSupport, PersonalProfile, PublicationAttributes, ResourceAttributes, SecurityAttributes}
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
 *  version Apr. 25, 2026
 * @version Jun. 18, 2026
 * @author  ASAMI, Tomoharu
 */
final class UserAccountDataStoreUpdateSpec extends AnyWordSpec with Matchers {
  private def _lifecycle_attributes: LifecycleAttributes =
    LifecycleAttributes(
      java.time.Instant.EPOCH,
      java.time.Instant.EPOCH,
      Identifier("system"),
      Identifier("system"),
      org.simplemodeling.model.statemachine.PostStatus.default,
      org.simplemodeling.model.statemachine.Aliveness.default
    )

  "test-owned datastore" should {
    "persist direct updates for a seeded user account" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("probe")
      _seed_user_account(userid, "probe_principal", "probe@example.com").toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userid).toOption.get
      val eid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userid).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get

      datastore.update(
        cid,
        eid,
        Record.dataAuto(
          "emailVerifiedAt" -> "2026-04-07T00:00:00Z",
          "phoneNumber" -> "09012345678"
        )
      ).toOption.isDefined shouldBe true

      val stored = datastore.load(cid, eid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getString("emailVerifiedAt")) shouldBe Some("2026-04-07T00:00:00Z")
        stored.flatMap(_.getString("phoneNumber")) shouldBe Some("09012345678")
      }
    }

    "persist direct datastore updates executed through ActionEngine" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val component = _component()
      val userid = _user_account_id("probe_action")
      _seed_user_account(userid, "probe_principal", "probe-action@example.com").toOption.isDefined shouldBe true

      val action = _ProbeUpdateAction(userid)
      val call = component.logic.createActionCall(action, fixture.executionContext)
      component.actionEngine.execute(call).toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userid).toOption.get
      val eid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userid).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, eid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getString("emailVerifiedAt")) shouldBe Some("2026-04-07T00:00:00Z")
      }
    }

    "persist typed patch updates through entity_update action execution" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("typed_update")
      _seed_user_account(userid, "probe_principal", "typed-update@example.com").toOption.isDefined shouldBe true

      val action = _ProbeEntityPatchAction(userid)
      val managerctx = _with_security(
        fixture,
        "probe_principal",
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      val call = action.createCall(ActionCall.Core(action, managerctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userid).toOption.get
      val dsid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userid).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, dsid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getAny("emailVerifiedAt")) should not be empty
      }
    }

    "normalize datastore key aliases with last non-null value precedence" in {
      val compatible = ComponentFactory.normalizeDataStoreRecord(
        Record.dataAuto(
          "email_verified_at" -> "2026-04-07T00:00:00Z",
          "emailVerifiedAt" -> "2026-04-07T00:00:00Z"
        )
      )
      compatible.getString("emailVerifiedAt") shouldBe Some("2026-04-07T00:00:00Z")

      val latest = ComponentFactory.normalizeDataStoreRecord(
        Record.dataAuto(
          "emailVerifiedAt" -> "2026-04-07T00:00:00Z",
          "email_verified_at" -> "2026-04-08T00:00:00Z"
        )
      )
      latest.getString("emailVerifiedAt") shouldBe Some("2026-04-08T00:00:00Z")
    }

    "execute login with a manually built UserLogin" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_manual")
      _seed_user_account(userid, ownerprincipalid, "integration-manual@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
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
      val action = org.simplemodeling.textus.useraccount.UserAccountComponent.UserService.UserLogin.unsafeForTest(
        request,
        request.toRecord
      )
      val call = component.logic.createActionCall(action, anonymousctx)
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
      val sessions = {
        val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
        summon[ExecutionContext].dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      }
      sessions.count(_.getString("userAccountId").contains(userid.print)) shouldBe 1
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("accessSessionId") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("refreshSessionId") should not be empty
      val accesssessionid = response.getString("accessSessionId").getOrElse(fail("missing accessSessionId"))
      val refreshsessionid = response.getString("refreshSessionId").getOrElse(fail("missing refreshSessionId"))
      accesssessionid.length should be <= 64
      refreshsessionid.length should be <= 64
      EntityId.parse(accesssessionid).toOption should not be empty
      EntityId.parse(refreshsessionid).toOption should not be empty
    }

    "execute login under an authenticated caller context without inheriting the caller principal into provider sessions" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_authenticated_caller")
      _seed_user_account(userid, ownerprincipalid, "integration-authenticated@example.com", loginName = Some("authenticated-login")).toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val staleaccesssessionid = _access_session_id("stale_authenticated_caller").print
      val authenticatedctx = _with_security(
        fixture,
        userid.print,
        extraAttributes = Map("accessToken" -> staleaccesssessionid)
      )
      val result = _execute_request(
        component,
        authenticatedctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("username", "authenticated-login", None),
            Property("password", "secret", None)
          )
        )
      )

      result.toOption.isDefined shouldBe true
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessSessionId") should not be empty
      response.getString("refreshSessionId") should not be empty
      val accesscid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val refreshcid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val accessrecords = summon[ExecutionContext].dataStoreSpace.search(accesscid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val refreshrecords = summon[ExecutionContext].dataStoreSpace.search(refreshcid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      accessrecords.count(_.getString("userAccountId").contains(userid.print)) shouldBe 1
      refreshrecords.count(_.getString("userAccountId").contains(userid.print)) shouldBe 1
    }

    "execute login after side-effect update wiring" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_only")
      _seed_user_account(
        userid,
        ownerprincipalid,
        "integration@example.com",
        phoneNumber = Some("09012345678")
      ).toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val result = _execute_request(
        component,
        anonymousctx,
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

    "register a user with loginName and resolve it by loginName lookup" in {
      val fixture = _fixture()
      val component = _component()
      val anonymousctx = _with_security(
        fixture,
        "register_lookup_owner",
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val registered = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "register",
          properties = List(
            Property("name", "Alice", None),
            Property("title", "member", None),
            Property("loginName", "alice", None),
            Property("email", "alice@example.com", None),
            Property("password", "secret", None),
            Property("locale", "ja-JP", None),
            Property("timeZone", "Asia/Tokyo", None)
          )
        )
      )
      registered.toOption.isDefined shouldBe true

      val lookedup = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "lookupUserByLoginName",
          properties = List(Property("loginName", "alice", None))
        )
      )
      lookedup.toOption.isDefined shouldBe true
      val response = lookedup.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("loginName").orElse(response.getString("loginName")) shouldBe Some("alice")
      response.getString("email") shouldBe Some("alice@example.com")
      response.getString("locale") shouldBe Some("ja-JP")
      response.getString("timeZone").orElse(response.getString("timeZone")) shouldBe Some("Asia/Tokyo")
    }

    "execute login for a registered user" in {
      val fixture = _fixture()
      val component = _component()
      val anonymousctx = _with_security(
        fixture,
        "register_login_owner",
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val registered = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "register",
          properties = List(
            Property("name", "Bob", None),
            Property("title", "member", None),
            Property("loginName", "bob", None),
            Property("email", "bob@example.com", None),
            Property("password", "secret", None)
          )
        )
      )
      registered.toOption.isDefined shouldBe true

      val login = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("identifier", "bob", None),
            Property("password", "secret", None)
          )
        )
      )
      login.toOption.isDefined shouldBe true
      val response = login.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("accessSessionId") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("userAccountId") should not be empty
    }

    "execute login for a registered user in SQLite datastore mode" in {
      val fixture = _sqlite_fixture()
      val component = _component()
      val anonymousctx = _with_security(
        fixture,
        "register_login_sqlite_owner",
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val registered = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "register",
          properties = List(
            Property("name", "Sqlite Bob", None),
            Property("title", "member", None),
            Property("loginName", "sqlite-bob", None),
            Property("email", "sqlite-bob@example.com", None),
            Property("password", "secret", None)
          )
        )
      )
      registered.toOption.isDefined shouldBe true

      val login = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("identifier", "sqlite-bob", None),
            Property("password", "secret", None)
          )
        )
      )
      login.toOption.isDefined shouldBe true
      val response = login.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("accessSessionId") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("userAccountId") should not be empty
    }

    "reject loginName lookup for suspended accounts" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("lookup_suspended")
      _seed_user_account(
        userid,
        "lookup_suspended_owner",
        "lookup-suspended@example.com",
        loginName = Some("suspended-user"),
        status = UserAccountStatus.Suspended
      ).toOption.isDefined shouldBe true

      val lookedup = _execute_request(
        component,
        fixture.executionContext,
        Request.ofService(
          "User",
          "lookupUserByLoginName",
          properties = List(Property("loginName", "suspended-user", None))
        )
      )
      lookedup.isSuccess shouldBe false
    }

    "resolve current user id from persisted access session token" in {
      val fixture = _fixture()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "session_owner"
      val userid = _user_account_id("session_current")
      val token = "token-session_owner"
      _seed_user_account(userid, ownerprincipalid, "session-current@example.com").toOption.isDefined shouldBe true
      _seed_access_session(userid, ownerprincipalid, token).toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(
        fixture,
        ownerprincipalid,
        extraAttributes = Map("accessToken" -> token)
      )
      ComponentFactory.currentLoggedInUserId()(using authenticatedctx).toOption shouldBe Some(userid)
    }

    "load user account aggregate with its user profile" in {
      val component = _component()
      val ownerprincipalid = "aggregate_owner"
      val ownerctx = ExecutionContext.withSecurityContext(component.logic.executionContext(), _security_context(ownerprincipalid))
      given ExecutionContext = ownerctx
      val userid = _user_account_id("aggregate_profile")
      _seed_user_account(userid, ownerprincipalid, "aggregate-profile@example.com").toOption.isDefined shouldBe true
      _seed_user_profile(userid, ownerprincipalid).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        ownerctx,
        Request.ofService(
          "aggregate",
          "loadUserAccount",
          properties = List(Property("id", userid.print, None))
        )
      )
      result.toOption.isDefined shouldBe true
    }

    "load user profile view" in {
      val component = _component()
      val ownerprincipalid = "view_owner"
      val managerctx = ExecutionContext.withSecurityContext(
        component.logic.executionContext(),
        _security_context("manager_principal", privilege = SecurityContext.Privilege.ApplicationContentManager)
      )
      given ExecutionContext = managerctx
      val userid = _user_account_id("view_profile")
      val profileid = _user_profile_id("view_profile")
      _seed_user_account(userid, ownerprincipalid, "view-profile@example.com").toOption.isDefined shouldBe true
      _seed_user_profile(userid, ownerprincipalid, Some(profileid)).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        managerctx,
        Request.ofService(
          "view",
          "loadUserProfile",
          properties = List(Property("id", profileid.print, None))
        )
      )
      result.toOption.isDefined shouldBe true
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      val identitypresentation = ComponentFactory.normalizeDataStoreRecord(
        response.getRecord("identityPresentation").getOrElse(fail("identityPresentation missing"))
      )
      identitypresentation.getString("displayName") shouldBe Some("Integration User")
      identitypresentation.getString("nickname") shouldBe Some("integ-user")
      identitypresentation.getString("avatarUrl") shouldBe Some("https://example.com/avatar/integration-user.png")
    }

    "logout should revoke only the current session pair" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "logout_current_owner"
      val userid = _user_account_id("logout_current")
      val currentaccesstoken = "access-token-current"
      val currentrefreshtoken = "refresh-token-current"
      val otheraccesstoken = "access-token-other"
      val otherrefreshtoken = "refresh-token-other"
      _seed_user_account(userid, ownerprincipalid, "logout-current@example.com").toOption.isDefined shouldBe true
      val currentrefreshid = _seed_refresh_session(userid, ownerprincipalid, currentrefreshtoken).toOption.get
      _seed_access_session(userid, ownerprincipalid, currentaccesstoken, Some(currentrefreshid)).toOption.isDefined shouldBe true
      _seed_refresh_session(userid, ownerprincipalid, otherrefreshtoken).toOption.isDefined shouldBe true
      _seed_access_session(userid, ownerprincipalid, otheraccesstoken, None).toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(
        fixture,
        ownerprincipalid,
        extraAttributes = Map(
          "accessToken" -> currentaccesstoken,
          "refreshToken" -> currentrefreshtoken
        )
      )
      val result = _execute_request(
        component,
        authenticatedctx,
        Request.ofService(
          "User",
          "logout",
          properties = List(Property("userAccountId", userid.print, None))
        )
      )
      result.toOption.isDefined shouldBe true

      val refreshcid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshrecords = summon[ExecutionContext].dataStoreSpace.search(refreshcid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val revokedrefreshcount = refreshrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").nonEmpty)
      revokedrefreshcount shouldBe 1
      val activerefreshcount = refreshrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activerefreshcount shouldBe 1

      val accesscid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessrecords = summon[ExecutionContext].dataStoreSpace.search(accesscid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val revokedaccesscount = accessrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").nonEmpty)
      revokedaccesscount shouldBe 1
      val activeaccesscount = accessrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activeaccesscount shouldBe 1
    }

    "logoutAll should revoke all session pairs for the account" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "logout_all_owner"
      val userid = _user_account_id("logout_all")
      val currentaccesstoken = "access-token-all-current"
      val currentrefreshtoken = "refresh-token-all-current"
      _seed_user_account(userid, ownerprincipalid, "logout-all@example.com").toOption.isDefined shouldBe true
      val refreshid1 = _seed_refresh_session(userid, ownerprincipalid, currentrefreshtoken).toOption.get
      _seed_access_session(userid, ownerprincipalid, currentaccesstoken, Some(refreshid1)).toOption.isDefined shouldBe true
      _seed_refresh_session(userid, ownerprincipalid, "refresh-token-all-other").toOption.isDefined shouldBe true
      _seed_access_session(userid, ownerprincipalid, "access-token-all-other", None).toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(
        fixture,
        ownerprincipalid,
        extraAttributes = Map(
          "accessToken" -> currentaccesstoken,
          "refreshToken" -> currentrefreshtoken
        )
      )
      val result = _execute_request(
        component,
        authenticatedctx,
        Request.ofService(
          "User",
          "logoutAll",
          properties = List(Property("userAccountId", userid.print, None))
        )
      )
      result.toOption.isDefined shouldBe true

      val refreshcid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshrecords = summon[ExecutionContext].dataStoreSpace.search(refreshcid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val activerefreshcount = refreshrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activerefreshcount shouldBe 0

      val accesscid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessrecords = summon[ExecutionContext].dataStoreSpace.search(accesscid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val activeaccesscount = accessrecords.count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activeaccesscount shouldBe 0
    }

    "rotate refresh token and issue a new token pair" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "refresh_owner"
      val userid = _user_account_id("refresh_pair")
      val refreshtoken = "refresh-token-owner"
      _seed_user_account(userid, ownerprincipalid, "refresh@example.com").toOption.isDefined shouldBe true
      val originalrefreshid = _seed_refresh_session(userid, ownerprincipalid, refreshtoken).toOption.get

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val result = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshtoken, None))
        )
      )
      result.toOption.isDefined shouldBe true
      val response = result.toOption.get.asInstanceOf[RecordResponse].record
      response.getString("accessToken") should not be empty
      response.getString("refreshToken") should not be empty
      response.getString("refreshToken") should not be Some(refreshtoken)

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val records = summon[ExecutionContext].dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val previous = records.find(_.getString("id").contains(originalrefreshid.print)).get
      previous.getString("rotatedAt") should not be empty
      previous.getString("successorSessionId") should not be empty
      records.count(_.getString("userAccountId").contains(userid.print)) shouldBe 2
    }

    "detect refresh token reuse and revoke remaining sessions" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "refresh_reuse_owner"
      val userid = _user_account_id("refresh_reuse")
      val refreshtoken = "refresh-token-reuse"
      _seed_user_account(userid, ownerprincipalid, "reuse@example.com").toOption.isDefined shouldBe true
      _seed_refresh_session(userid, ownerprincipalid, refreshtoken).toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val first = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshtoken, None))
        )
      )
      first.toOption.isDefined shouldBe true

      val second = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "refreshAccessToken",
          properties = List(Property("refreshToken", refreshtoken, None))
        )
      )
      second.toOption.isDefined shouldBe false

      val refreshcid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshrecords = summon[ExecutionContext].dataStoreSpace.search(refreshcid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val activerefreshcount = refreshrecords
        .count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activerefreshcount shouldBe 0

      val accesscid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessrecords = summon[ExecutionContext].dataStoreSpace.search(accesscid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val activeaccesscount = accessrecords
        .count(r => r.getString("userAccountId").contains(userid.print) && r.getString("revokedAt").isEmpty)
      activeaccesscount shouldBe 0
    }

    "authenticate through provider login and restore the same session id" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("provider_login")
      _seed_user_account(userid, "provider_owner", "provider-login@example.com", locale = Some("ja-JP"), timeZone = Some("Asia/Tokyo")).toOption.isDefined shouldBe true
      _seed_credential(userid, "provider_owner", "secret").toOption.isDefined shouldBe true

      val provider = component.authenticationProviders.head
      val login = provider.login(AuthenticationRequest(Map(
        "username" -> "provider-login@example.com",
        "password" -> "secret"
      )))
      login.toOption.flatten should not be empty
      val authenticated = login.toOption.flatten.get
      val sessionid = authenticated.session.flatMap(_.sessionId).getOrElse(fail("session id missing"))
      authenticated.attributes.get("accessToken") shouldBe empty
      authenticated.attributes.get("locale") shouldBe Some("ja-JP")
      authenticated.attributes.get("timeZone") shouldBe Some("Asia/Tokyo")
      authenticated.session.flatMap(_.attributes.get("locale")) shouldBe Some("ja-JP")
      authenticated.session.flatMap(_.attributes.get("timeZone")) shouldBe Some("Asia/Tokyo")

      val restored = provider.authenticate(AuthenticationRequest(Map(
        "x-textus-session" -> sessionid
      )))
      restored.toOption.flatten should not be empty
      restored.toOption.flatten.get.principalId.value shouldBe userid.parts.entropy
      restored.toOption.flatten.get.principalId.value.length should be <= 64
      restored.toOption.flatten.get.attributes.get("userAccountId") shouldBe Some(userid.print)
      restored.toOption.flatten.get.attributes.get("locale") shouldBe Some("ja-JP")
      restored.toOption.flatten.get.attributes.get("timeZone") shouldBe Some("Asia/Tokyo")
      restored.toOption.flatten.get.session.flatMap(_.sessionId) shouldBe Some(sessionid)
    }

    "seed debug account only when debug auth is enabled" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _component(_debug_auth_nested_configuration(
        seed = true,
        autologin = false,
        loginname = "debug-seed-test",
        email = "debug-seed-test@example.com",
        password = "debug-seed-password"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val login = provider.login(AuthenticationRequest(Map(
        "username" -> "debug-seed-test",
        "password" -> "debug-seed-password"
      )))

      login.toOption.flatten should not be empty
      login.toOption.flatten.get.attributes.get("loginName") shouldBe Some("debug-seed-test")
      login.toOption.flatten.get.session.flatMap(_.sessionId) should not be empty
    }

    "seed debug account after collection bootstrap" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _bootstrapped_component(_debug_auth_nested_configuration(
        seed = true,
        autologin = false,
        loginname = "debug-bootstrap-test",
        email = "debug-bootstrap-test@example.com",
        password = "debug-bootstrap-password"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val login = provider.login(AuthenticationRequest(Map(
        "username" -> "debug-bootstrap-test",
        "password" -> "debug-bootstrap-password"
      )))

      login.toOption.flatten should not be empty
      login.toOption.flatten.get.attributes.get("loginName") shouldBe Some("debug-bootstrap-test")
    }

    "keep an existing debug user credential unchanged" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _bootstrapped_component(_debug_auth_nested_configuration(
        seed = true,
        autologin = false,
        loginname = "debug-existing-test",
        email = "debug-existing-test@example.com",
        password = "debug-new-password"
      ))
      given ExecutionContext = component.logic.executionContext()
      val userid = _user_account_id("debug_existing")
      _seed_user_account(
        userid,
        "debug_existing_owner",
        "debug-existing-test@example.com",
        loginName = Some("debug-existing-test")
      ).toOption.isDefined shouldBe true
      _seed_credential(userid, "debug_existing_owner", "debug-old-password").toOption.isDefined shouldBe true
      val provider = component.authenticationProviders.head

      val oldlogin = provider.login(AuthenticationRequest(Map(
        "username" -> "debug-existing-test",
        "password" -> "debug-old-password"
      )))
      val newlogin = provider.login(AuthenticationRequest(Map(
        "username" -> "debug-existing-test",
        "password" -> "debug-new-password"
      )))

      oldlogin.toOption.flatten should not be empty
      newlogin.toOption.flatten shouldBe empty
    }

    "keep test login disabled without debug auth configuration" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _component()
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val login = provider.login(AuthenticationRequest(Map(
        "username" -> "test",
        "password" -> "test"
      )))

      login.toOption.flatten shouldBe empty
    }

    "auto-login restores a real debug access session" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _component(_debug_auth_configuration(
        seed = true,
        autologin = true,
        loginname = "debug-auto-test",
        email = "debug-auto-test@example.com"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val current = provider.currentSession(AuthenticationRequest(Map.empty))

      current.toOption.flatten should not be empty
      current.toOption.flatten.get.attributes.get("loginName") shouldBe Some("debug-auto-test")
      current.toOption.flatten.get.session.flatMap(_.sessionId) should not be empty
    }

    "auto-login is allowed in demo mode" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _component(_debug_auth_configuration(
        seed = true,
        autologin = true,
        operationmode = "demo",
        loginname = "demo",
        email = "demo@example.com"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val current = provider.currentSession(AuthenticationRequest(Map.empty))

      current.toOption.flatten should not be empty
      current.toOption.flatten.get.attributes.get("loginName") shouldBe Some("demo")
      current.toOption.flatten.get.session.flatMap(_.sessionId) should not be empty
    }

    "auto-login restores a real debug access session after collection bootstrap" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _bootstrapped_component(_debug_auth_configuration(
        seed = true,
        autologin = true,
        loginname = "debug-auto-bootstrap-test",
        email = "debug-auto-bootstrap-test@example.com"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val current = provider.currentSession(AuthenticationRequest(Map.empty))

      current.toOption.flatten should not be empty
      current.toOption.flatten.get.attributes.get("loginName") shouldBe Some("debug-auto-bootstrap-test")
      current.toOption.flatten.get.session.flatMap(_.sessionId) should not be empty
    }

    "auto-login recovers when a stale browser session cookie is present" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()
      val component = _component(_debug_auth_configuration(
        seed = true,
        autologin = true,
        loginname = "debug-auto-stale-session-test",
        email = "debug-auto-stale-session-test@example.com"
      ))
      given ExecutionContext = component.logic.executionContext()
      val provider = component.authenticationProviders.head

      val current = provider.currentSession(AuthenticationRequest(Map(
        "x-textus-session" -> "missing-session"
      )))

      current.toOption.flatten should not be empty
      current.toOption.flatten.get.attributes.get("loginName") shouldBe Some("debug-auto-stale-session-test")
      current.toOption.flatten.get.session.flatMap(_.sessionId) should not be empty
    }

    "reject debug auth in production mode during component initialization" in {
      ComponentFactory.resetEphemeralSecurityStateForTest()

      val thrown = intercept[IllegalArgumentException] {
        _component(_debug_auth_configuration(seed = true, autologin = false, operationmode = "production"))
      }
      thrown.getMessage should include ("textus.debug.auth.enabled")
    }

    "return provider-backed current session summary from session id" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("provider_current_session")
      _seed_user_account(userid, "provider_owner", "provider-current@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, "provider_owner", "secret").toOption.isDefined shouldBe true

      val provider = component.authenticationProviders.head
      val login = provider.login(AuthenticationRequest(Map(
        "email" -> "provider-current@example.com",
        "password" -> "secret"
      )))
      val sessionid = login.toOption.flatten.flatMap(_.session.flatMap(_.sessionId)).getOrElse(fail("session id missing"))

      val current = provider.currentSession(AuthenticationRequest(Map(
        "x-textus-session" -> sessionid
      )))
      current.toOption.flatten should not be empty
      current.toOption.flatten.get.principalId.value shouldBe userid.parts.entropy
      current.toOption.flatten.get.principalId.value.length should be <= 64
      current.toOption.flatten.get.attributes.get("userAccountId") shouldBe Some(userid.print)
      current.toOption.flatten.get.session.flatMap(_.sessionId) shouldBe Some(sessionid)
    }

    "revoke provider session on logout so subsequent restore fails" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("provider_logout")
      _seed_user_account(userid, "provider_owner", "provider-logout@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, "provider_owner", "secret").toOption.isDefined shouldBe true

      val provider = component.authenticationProviders.head
      val sessionid = provider.login(AuthenticationRequest(Map(
        "email" -> "provider-logout@example.com",
        "password" -> "secret"
      ))).toOption.flatten.flatMap(_.session.flatMap(_.sessionId)).getOrElse(fail("session id missing"))

      provider.logout(AuthenticationRequest(Map(
        "x-textus-session" -> sessionid
      ))).toOption.flatten.flatMap(_.sessionId) shouldBe Some(sessionid)

      provider.authenticate(AuthenticationRequest(Map(
        "x-textus-session" -> sessionid
      ))).toOption.flatten.isDefined shouldBe false
    }

    "treat invalid provider session deterministically without throwing" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val provider = component.authenticationProviders.head

      val result = provider.authenticate(AuthenticationRequest(Map(
        "x-textus-session" -> "missing-session"
      )))

      result.toOption shouldBe Some(None)
    }

    "treat invalid provider current session as anonymous" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val provider = component.authenticationProviders.head

      val result = provider.currentSession(AuthenticationRequest(Map(
        "x-textus-session" -> "missing-session"
      )))

      result.toOption shouldBe Some(None)
    }

    "treat missing stored provider current session as anonymous" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val provider = component.authenticationProviders.head
      val sessionid = _access_session_id("missing_stored_current_session").print

      val result = provider.currentSession(AuthenticationRequest(Map(
        "x-textus-session" -> sessionid
      )))

      result.toOption shouldBe Some(None)
    }

    "treat invalid provider logout as already cleared" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val provider = component.authenticationProviders.head

      val result = provider.logout(AuthenticationRequest(Map(
        "x-textus-session" -> "missing-session"
      )))

      result.toOption.isDefined shouldBe true
      result.toOption.flatten shouldBe empty
    }

    "rotate internal token state while preserving external session id" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val userid = _user_account_id("provider_rotate")
      _seed_user_account(userid, "provider_owner", "provider-rotate@example.com").toOption.isDefined shouldBe true
      val refreshid = _seed_refresh_session(userid, "provider_owner", "refresh-token-provider").toOption.get
      val accessid = _seed_access_session(userid, "provider_owner", "access-token-provider", Some(refreshid)).toOption.get

      val accesscid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(AccessSessionQuery.collectionId).toOption.get
      val accessdsid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(accessid).toOption.get
      val accessdatastore = summon[ExecutionContext].dataStoreSpace.dataStore(accesscid).toOption.get
      accessdatastore.update(
        accesscid,
        accessdsid,
        Record.dataAuto("expiresAt" -> "2026-04-01T00:00:00Z")
      ).toOption.isDefined shouldBe true

      val provider = component.authenticationProviders.head
      val restored = provider.authenticate(AuthenticationRequest(Map(
        "x-textus-session" -> accessid.print
      )))
      restored.toOption.flatten should not be empty
      restored.toOption.flatten.get.session.flatMap(_.sessionId) shouldBe Some(accessid.print)

      val refreshcid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(RefreshSessionQuery.collectionId).toOption.get
      val refreshrecords = summon[ExecutionContext].dataStoreSpace.search(refreshcid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty)).toOption.get.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
      val previous = refreshrecords.find(_.getString("id").contains(refreshid.print)).getOrElse(fail("previous refresh missing"))
      previous.getString("rotatedAt") should not be empty
      previous.getString("successorSessionId") should not be empty

      val rotatedaccess = accessdatastore.load(accesscid, accessdsid).toOption.flatten
        .map(ComponentFactory.normalizeDataStoreRecord)
        .getOrElse(fail("rotated access missing"))
      rotatedaccess.getString("refreshSessionId") should not be Some(refreshid.print)
      rotatedaccess.getString("revokedAt") shouldBe empty
    }

    "persist login-like patch after raw entity lookup under owner context without access token" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_patch")
      _seed_user_account(userid, ownerprincipalid, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeEntityPatchAction(userid)
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist current login-like typed load and working-set side effect path" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_workingset")
      _seed_user_account(userid, ownerprincipalid, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeWorkingSetUpsertAction(userid)
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist login-like working-set put plus login-shaped response" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_shape")
      _seed_user_account(userid, ownerprincipalid, "integration@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeLoginWorkingSetOnlyAction(userid)
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist combined login side effects outside the real login operation path" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_side_effects")
      _seed_user_account(userid, ownerprincipalid, "integration@example.com").toOption.isDefined shouldBe true

      val managerctx = _with_security(
        fixture,
        ownerprincipalid,
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      val action = _ProbeLoginSideEffectsAction(userid)
      val call = action.createCall(ActionCall.Core(action, managerctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "keep working when only raw user lookup is included in a procedure probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_user_proc")
      _seed_user_account(userid, ownerprincipalid, "integration-user-proc@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawUserLookupProcedureAction("integration-user-proc@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "keep working when only raw user lookup is included in a probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_user_only")
      _seed_user_account(userid, ownerprincipalid, "integration-user-only@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawUserLookupLoginAction("integration-user-only@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "show recovered entity id from raw user lookup record" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_id_compare")
      _seed_user_account(userid, ownerprincipalid, "integration-id-compare@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeCompareRecoveredUserIdAction(userid, "integration-id-compare@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "show raw user-account search records before typed lookup" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_dump")
      _seed_user_account(userid, ownerprincipalid, "integration-dump@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeDumpRawUserRecordsAction("integration-dump@example.com")
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "surface current duplicate failure when only typed user load is included in a probe action" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_typed_load")
      _seed_user_account(userid, ownerprincipalid, "integration-typed-load@example.com").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeTypedUserLoadAction(userid)
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist entity update in a raw lookup probe action under owner context without access token" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("login_probe_raw")
      _seed_user_account(userid, ownerprincipalid, "integration-raw@example.com").toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val anonymousctx = _with_security(
        fixture,
        ownerprincipalid,
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )
      val action = _ProbeRawLookupLoginAction("integration-raw@example.com", "secret")
      val call = action.createCall(ActionCall.Core(action, anonymousctx, Some(component), None))
      val result = component.actionEngine.execute(call)
      result.toOption.isDefined shouldBe true
    }

    "persist verification updates through actual component operations" in {
      val fixture = _fixture()
      val component = _component()
      given ExecutionContext = fixture.executionContext
      val ownerprincipalid = "integration_owner"
      val userid = _user_account_id("integration")
      _seed_user_account(
        userid,
        ownerprincipalid,
        "integration@example.com",
        phoneNumber = Some("09012345678")
      ).toOption.isDefined shouldBe true
      _seed_credential(userid, ownerprincipalid, "secret").toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(fixture, userid.print)
      ComponentFactory.addLoggedInUserForTest(userid)(using authenticatedctx).toOption.isDefined shouldBe true
      _execute_request(
        component,
        authenticatedctx,
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
        authenticatedctx,
        Request.ofService(
          "User",
          "verifyMyPhone",
          properties = List(
            Property("phoneNumber", "09012345678", None),
            Property("proofToken", "phone-proof", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val cid = fixture.executionContext.entityStoreSpace.dataStoreCollection(userid).toOption.get
      val dsid = fixture.executionContext.entityStoreSpace.dataStoreEntryId(userid).toOption.get
      val datastore = fixture.executionContext.dataStoreSpace.dataStore(cid).toOption.get
      val stored = datastore.load(cid, dsid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      withClue(s"stored=${stored.map(_.fields.map(f => f.key -> f.value).toVector)}") {
        stored.flatMap(_.getAny("emailVerifiedAt")) should not be empty
        stored.flatMap(_.getAny("phoneVerifiedAt")) should not be empty
        stored.flatMap(_.getString("phoneNumber")) shouldBe Some("09012345678")
        stored.flatMap(_.getAny("lastLoginAt")) shouldBe empty
      }
    }

    "reject email verification when the current account email is already verified" in {
      val fixture = _fixture()
      val component = _component()
      val userid = _user_account_id("already_verified_email")
      val ownerprincipalid = "already_verified_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerprincipalid))
      _seed_user_account(
        userid,
        ownerprincipalid,
        "already-verified-email@example.com",
        emailVerifiedAt = Some(java.time.Instant.parse("2026-04-08T00:00:00Z"))
      ).toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(fixture, userid.print)
      ComponentFactory.addLoggedInUserForTest(userid)(using authenticatedctx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedctx,
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
      val userid = _user_account_id("missing_phone")
      val ownerprincipalid = "missing_phone_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerprincipalid))
      _seed_user_account(userid, ownerprincipalid, "missing-phone@example.com").toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(fixture, userid.print)
      ComponentFactory.addLoggedInUserForTest(userid)(using authenticatedctx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedctx,
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
      val userid = _user_account_id("phone_mismatch")
      val ownerprincipalid = "phone_mismatch_owner"
      given ExecutionContext = fixture.contextFor(_security_context(ownerprincipalid))
      _seed_user_account(
        userid,
        ownerprincipalid,
        "phone-mismatch@example.com",
        phoneNumber = Some("09000000000")
      ).toOption.isDefined shouldBe true

      val authenticatedctx = _with_security(fixture, userid.print)
      ComponentFactory.addLoggedInUserForTest(userid)(using authenticatedctx).toOption.isDefined shouldBe true

      val result = _execute_request(
        component,
        authenticatedctx,
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
      val managerctx = _with_security(
        fixture,
        SecurityContext.Privilege.ApplicationContentManager.principalId.value,
        privilege = SecurityContext.Privilege.ApplicationContentManager
      )
      given ExecutionContext = managerctx
      val userid = _user_account_id("suspend_restore")
      _seed_user_account(userid, "integration_owner", "suspend-restore@example.com").toOption.isDefined shouldBe true

      _execute_request(
        component,
        managerctx,
        Request.ofService(
          "Management",
          "updateUserStatus",
          properties = List(
            Property("userAccountId", userid.print, None),
            Property("status", "suspended", None),
            Property("suspensionReason", "policy_violation", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val cid = summon[ExecutionContext].entityStoreSpace.dataStoreCollection(userid).toOption.get
      val dsid = summon[ExecutionContext].entityStoreSpace.dataStoreEntryId(userid).toOption.get
      val datastore = summon[ExecutionContext].dataStoreSpace.dataStore(cid).toOption.get
      val suspended = datastore.load(cid, dsid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      val suspendedsummary = suspended.map(_.fields.map(f => f.key -> Option(f.value).map(_.toString).orNull).toVector)
      withClue(s"suspended=${suspendedsummary}") {
        suspended.flatMap(_.getAny("status")).map(_.toString.contains("suspended")) shouldBe Some(true)
        suspended.flatMap(_.getAny("suspendedAt")) should not be empty
        suspended.flatMap(_.getString("suspendedBy")) shouldBe Some(SecurityContext.Privilege.ApplicationContentManager.principalId.value)
        suspended.flatMap(_.getString("suspensionReason")) shouldBe Some("policy_violation")
      }

      _execute_request(
        component,
        managerctx,
        Request.ofService(
          "Management",
          "updateUserStatus",
          properties = List(
            Property("userAccountId", userid.print, None),
            Property("status", "registered", None)
          )
        )
      ).toOption.isDefined shouldBe true

      val restored = datastore.load(cid, dsid).toOption.flatten.map(ComponentFactory.normalizeDataStoreRecord)
      val restoredsummary = restored.map(r =>
        Vector(
          "status" -> r.getAny("status").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspendedAt" -> r.getAny("suspendedAt").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspendedBy" -> r.getAny("suspendedBy").flatMap(x => Option(x).map(_.toString)).orNull,
          "suspensionReason" -> r.getAny("suspensionReason").flatMap(x => Option(x).map(_.toString)).orNull
        )
      )
      withClue(s"restored=${restoredsummary}") {
        restored.flatMap(_.getAny("status")).map(_.toString.contains("registered")) shouldBe Some(true)
        restored.flatMap(_.getAny("suspendedAt")).forall(java.util.Objects.isNull) shouldBe true
        restored.flatMap(_.getAny("suspendedBy")).forall(java.util.Objects.isNull) shouldBe true
        restored.flatMap(_.getAny("suspensionReason")).forall(java.util.Objects.isNull) shouldBe true
      }
    }
  }


    "request password reset without leaking account existence and confirm it with the issued token" in {
      val fixture = _fixture()
      val component = _component()
      val anonymousctx = _with_security(
        fixture,
        "password_reset_owner",
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val registered = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "register",
          properties = List(
            Property("name", "Reset User", None),
            Property("title", "member", None),
            Property("loginName", "reset-user", None),
            Property("email", "reset-user@example.com", None),
            Property("password", "secret", None)
          )
        )
      )
      registered.toOption.isDefined shouldBe true

      val acceptedknown = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "requestPasswordReset",
          properties = List(Property("email", "reset-user@example.com", None))
        )
      )
      val acceptedunknown = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "requestPasswordReset",
          properties = List(Property("email", "missing@example.com", None))
        )
      )
      acceptedknown.toOption.isDefined shouldBe true
      acceptedunknown.toOption.isDefined shouldBe true
      val resetdelivery = MessageDeliveryStubComponent.deliveries.lastOption.getOrElse(fail("missing reset notification"))
      resetdelivery.recipient shouldBe "reset-user@example.com"
      val resettoken = resetdelivery.attributes.getOrElse("resetToken", fail("missing reset token"))

      val confirm = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "confirmPasswordReset",
          properties = List(
            Property("token", resettoken, None),
            Property("newPassword", "secret-2", None)
          )
        )
      )
      confirm.toOption.isDefined shouldBe true

      val oldlogin = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("username", "reset-user", None),
            Property("password", "secret", None)
          )
        )
      )
      oldlogin.isSuccess shouldBe false

      val newlogin = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("username", "reset-user", None),
            Property("password", "secret-2", None)
          )
        )
      )
      newlogin.toOption.isDefined shouldBe true
    }

    "enroll two factor and require challenge completion for subsequent login" in {
      val fixture = _fixture()
      val component = _component()
      val anonymousctx = _with_security(
        fixture,
        "two_factor_owner",
        withAccessToken = false,
        extraAttributes = Map("anonymous" -> "true")
      )

      val registered = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "register",
          properties = List(
            Property("name", "Two Factor User", None),
            Property("title", "member", None),
            Property("loginName", "twofactor", None),
            Property("email", "twofactor@example.com", None),
            Property("password", "secret", None)
          )
        )
      )
      registered.toOption.isDefined shouldBe true

      val loginbeforeenroll = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("username", "twofactor", None),
            Property("password", "secret", None)
          )
        )
      )
      loginbeforeenroll.toOption.isDefined shouldBe true
      val loginrecord = loginbeforeenroll.toOption.get.asInstanceOf[RecordResponse].record
      val userid = loginrecord.getAsC[EntityId]("userAccountId").toOption.flatten
        .map(_.print)
        .orElse(loginrecord.getString("userAccountId"))
        .getOrElse(fail("missing user account id"))
      val authenticatedctx = _with_security(fixture, "two_factor_owner", extraAttributes = Map("userAccountId" -> userid))

      val enrolled = _execute_request(
        component,
        authenticatedctx,
        Request.ofService("User", "enrollTwoFactor", properties = List(Property("userAccountId", userid, None)))
      )
      enrolled.toOption.isDefined shouldBe true
      MessageDeliveryStubComponent.deliveries.lastOption.map(_.subject) shouldBe Some(Some("Two-factor authentication enabled"))

      MessageDeliveryStubComponent.clearDeliveries()
      val challenged = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "login",
          properties = List(
            Property("username", "twofactor", None),
            Property("password", "secret", None)
          )
        )
      )
      challenged.toOption.isDefined shouldBe true
      val challengedrecord = challenged.toOption.get.asInstanceOf[RecordResponse].record
      challengedrecord.getBoolean("twoFactorRequired") shouldBe Some(true)
      val challengeid = challengedrecord.getString("challengeId").getOrElse(fail("missing challenge id"))
      val challengedelivery = MessageDeliveryStubComponent.deliveries.lastOption.getOrElse(fail("missing two-factor notification"))
      challengedelivery.recipient shouldBe "twofactor@example.com"
      val code = challengedelivery.attributes.getOrElse("code", fail("missing verification code"))

      val invalid = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "verifyTwoFactorLogin",
          properties = List(
            Property("challengeId", challengeid, None),
            Property("code", "000000", None)
          )
        )
      )
      invalid.isSuccess shouldBe false

      val verified = _execute_request(
        component,
        anonymousctx,
        Request.ofService(
          "User",
          "verifyTwoFactorLogin",
          properties = List(
            Property("challengeId", challengeid, None),
            Property("code", code, None)
          )
        )
      )
      verified.toOption.isDefined shouldBe true
      val verifiedrecord = verified.toOption.get.asInstanceOf[RecordResponse].record
      verifiedrecord.getString("accessSessionId") should not be empty
    }

  private def _fixture(): _Fixture = {
    _fixture(DataStore.inMemorySearchable())
  }

  private def _sqlite_fixture(): _Fixture = {
    _fixture(SqlDataStore.sqlite(":memory:", config = SqlDataStore.Config(normalizeColumnNames = true)))
  }

  private def _fixture(datastore: DataStore): _Fixture = {
    ComponentFactory.resetLoginWorkingSetForTest()
    ComponentFactory.resetEphemeralSecurityStateForTest()
    MessageDeliveryStubComponent.clearDeliveries()
    val datastorespace = new DataStoreSpace().addDataStore(datastore)
    val entitystorespace = org.goldenport.cncf.entity.EntityStoreSpace.create(
      ResolvedConfiguration(Configuration.empty, ConfigurationTrace.empty)
    )
    val base = ExecutionContext.create()
    val core = RuntimeContext.core(
      name = "textus-user-account-datastore-probe",
      parent = None,
      observabilityContext = base.observability,
      datastore = Some(DataStoreContext(datastorespace)),
      entitystore = Some(EntityStoreContext(entitystorespace))
    )
    val eventengine = org.goldenport.cncf.event.EventEngine.noop(DataStore.noop())
    def build(security: SecurityContext): ExecutionContext = {
      lazy val context: ExecutionContext = ExecutionContext.withSecurityContext(
        ExecutionContext.withRuntimeContext(base, runtime),
        security
      )
      lazy val uow = new org.goldenport.cncf.unitofwork.UnitOfWork(context, eventengine, org.goldenport.cncf.unitofwork.CommitRecorder.noop)
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
        org.goldenport.Consequence.operationInvalid("request", s"request did not resolve to action: $other")
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
            (if (withAccessToken) Map("accessToken" -> s"token-$principalId") else Map.empty) ++
            extraAttributes
      },
      capabilities = privilege.capabilities,
      level = privilege.level
    )
  }

  private def _seed_user_account(
    id: EntityId,
    ownerprincipalid: String,
    email: String,
    loginName: Option[String] = None,
    status: UserAccountStatus = UserAccountStatus.Registered,
    emailVerifiedAt: Option[java.time.Instant] = None,
    phoneNumber: Option[String] = None,
    phoneVerifiedAt: Option[java.time.Instant] = None,
    locale: Option[String] = None,
    timeZone: Option[String] = None
  )(using ExecutionContext) = {
    val principal = ObjectId(Identifier(ownerprincipalid))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = UserAccountCreateEntity(
      id = Some(id),
      nameAttributes = NameAttributes.simple(Name("owner")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      contentAttributes = ContentAttributes.empty,
      lifecycleAttributes = _lifecycle_attributes,
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      email = email,
      loginName = loginName,
      externalSubjectId = None,
      emailVerifiedAt = emailVerifiedAt,
      phoneNumber = phoneNumber,
      locale = locale,
      timeZone = timeZone,
      phoneVerifiedAt = phoneVerifiedAt,
      lastLoginAt = None,
      passwordChangedAt = None,
      suspendedAt = None,
      suspendedBy = None,
      suspensionReason = None,
      status = status
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _seed_credential(
    userid: EntityId,
    ownerprincipalid: String,
    password: String
  )(using ExecutionContext) = {
    val credentialid = _credential_id("credential")
    val principal = ObjectId(Identifier(ownerprincipalid))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = CredentialCreateEntity(
      id = Some(credentialid),
      nameAttributes = NameAttributes.simple(Name("credential")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      contentAttributes = ContentAttributes.empty,
      lifecycleAttributes = _lifecycle_attributes,
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      userAccountId = userid,
      passwordHash = _password_hash(password)
    )
    EntityStore.standard().create(entity).map(_ => ())
  }

  private def _seed_access_session(
    userid: EntityId,
    ownerprincipalid: String,
    token: String,
    refreshSessionId: Option[EntityId] = None
  )(using ExecutionContext) = {
    val sessionid = _access_session_id("access_session")
    val principal = ObjectId(Identifier(ownerprincipalid))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = AccessSessionCreateEntity(
      id = Some(sessionid),
      nameAttributes = NameAttributes.simple(Name("access_session")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      contentAttributes = ContentAttributes.empty,
      lifecycleAttributes = _lifecycle_attributes,
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      userAccountId = userid,
      refreshSessionId = refreshSessionId.map(_.print),
      tokenHash = _password_hash(token),
      issuedAt = java.time.Instant.parse("2026-04-09T00:00:00Z"),
      expiresAt = java.time.Instant.parse("2099-01-01T00:00:00Z"),
      revokedAt = None,
      lastAccessedAt = None,
      clientId = None,
      deviceInfo = None,
      ipAddress = None,
      userAgent = None
    )
    EntityStore.standard().create(entity).map(_ => sessionid)
  }

  private def _seed_user_profile(
    userid: EntityId,
    ownerprincipalid: String,
    profileid: Option[EntityId] = None
  )(using ExecutionContext) = {
    val id = profileid.getOrElse(_user_profile_id("user_profile"))
    val principal = ObjectId(Identifier(ownerprincipalid))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = UserProfileCreateEntity(
      id = Some(id),
      nameAttributes = NameAttributes.simple(Name("user_profile")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      contentAttributes = ContentAttributes.empty,
      lifecycleAttributes = _lifecycle_attributes,
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      userAccountId = userid,
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
    userid: EntityId,
    ownerprincipalid: String,
    token: String
  )(using ExecutionContext) = {
    val sessionid = _refresh_session_id("refresh_session")
    val principal = ObjectId(Identifier(ownerprincipalid))
    val rights = SecurityAttributes.Rights(
      SecurityAttributes.Rights.Permissions(read = true, write = true, execute = true),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false),
      SecurityAttributes.Rights.Permissions(read = true, write = false, execute = false)
    )
    val entity = org.simplemodeling.textus.useraccount.entity.create.RefreshSession(
      id = Some(sessionid),
      nameAttributes = NameAttributes.simple(Name("refresh_session")),
      descriptiveAttributes = DescriptiveAttributes.empty,
      contentAttributes = ContentAttributes.empty,
      lifecycleAttributes = _lifecycle_attributes,
      publicationAttributes = PublicationAttributes(None, None, None, None, None),
      securityAttributes = SecurityAttributes(principal, principal, rights, principal),
      resourceAttributes = ResourceAttributes(),
      auditAttributes = AuditAttributes(),
      mediaAttributes = MediaAttributes(None, Vector.empty, Vector.empty, Vector.empty, Vector.empty),
      contextualAttribute = ContextualAttributes(),
      userAccountId = userid,
      successorSessionId = None,
      tokenHash = _password_hash(token),
      issuedAt = java.time.Instant.parse("2026-04-09T00:00:00Z"),
      expiresAt = java.time.Instant.parse("2099-01-01T00:00:00Z"),
      revokedAt = None,
      rotatedAt = None,
      clientId = None,
      deviceInfo = None,
      ipAddress = None,
      userAgent = None
    )
    EntityStore.standard().create(entity).map(_ => sessionid)
  }

  private def _user_account_id(label: String): EntityId =
    _entity_id(UserAccountQuery.collectionId, label)

  private def _credential_id(label: String): EntityId =
    _entity_id(org.simplemodeling.textus.useraccount.entity.query.Credential.collectionId, label)

  private def _access_session_id(label: String): EntityId =
    ComponentFactory.generateSessionEntityId(AccessSessionQuery.collectionId)

  private def _refresh_session_id(label: String): EntityId =
    ComponentFactory.generateSessionEntityId(RefreshSessionQuery.collectionId)

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
    _component(Configuration.empty)
  }

  private def _component(configuration: Configuration): Component = {
    val subsystem = org.goldenport.cncf.subsystem.Subsystem(
      name = "textus-user-account-datastore-probe",
      configuration = ResolvedConfiguration(configuration, ConfigurationTrace.empty),
      aliasResolver = org.goldenport.cncf.path.AliasResolver.empty,
      runMode = org.goldenport.cncf.cli.RunMode.Command
    )
    val params = ComponentCreate(
      subsystem = subsystem,
      origin = ComponentOrigin.Builtin
    )
    val component = GeneratedDomainComponentLoader.create(params).head
    val notificationstub = MessageDeliveryStubComponent.Factory.create(ComponentCreate(subsystem = subsystem, origin = ComponentOrigin.Builtin)).participants.head
    subsystem.add(Vector(notificationstub, component))
    subsystem.components.find(_.name == component.name).getOrElse(component)
  }

  private def _bootstrapped_component(configuration: Configuration): Component =
    new org.goldenport.cncf.component.ComponentFactory().bootstrap(_component(configuration))

  private def _debug_auth_configuration(
    seed: Boolean,
    autologin: Boolean,
    operationmode: String = "develop",
    loginname: String = "test",
    email: String = "test@example.com",
    password: String = "test"
  ): Configuration =
    Configuration(Map(
      "textus.operation-mode" -> ConfigurationValue.StringValue(operationmode),
      "textus.debug.auth.enabled" -> ConfigurationValue.StringValue("true"),
      "textus.debug.auth.seed-account.enabled" -> ConfigurationValue.StringValue(seed.toString),
      "textus.debug.auth.auto-login.enabled" -> ConfigurationValue.StringValue(autologin.toString),
      "textus.debug.auth.account.login-name" -> ConfigurationValue.StringValue(loginname),
      "textus.debug.auth.account.email" -> ConfigurationValue.StringValue(email),
      "textus.debug.auth.account.password" -> ConfigurationValue.StringValue(password),
      "textus.debug.auth.account.status" -> ConfigurationValue.StringValue("registered")
    ))

  private def _debug_auth_nested_configuration(
    seed: Boolean,
    autologin: Boolean,
    loginname: String,
    email: String,
    password: String
  ): Configuration =
    Configuration(Map(
      "textus" -> ConfigurationValue.ObjectValue(Map(
        "operation-mode" -> ConfigurationValue.StringValue("develop"),
        "debug" -> ConfigurationValue.ObjectValue(Map(
          "auth" -> ConfigurationValue.ObjectValue(Map(
            "enabled" -> ConfigurationValue.BooleanValue(true),
            "seed-account" -> ConfigurationValue.ObjectValue(Map(
              "enabled" -> ConfigurationValue.BooleanValue(seed)
            )),
            "auto-login" -> ConfigurationValue.ObjectValue(Map(
              "enabled" -> ConfigurationValue.BooleanValue(autologin)
            )),
            "account" -> ConfigurationValue.ObjectValue(Map(
              "login-name" -> ConfigurationValue.StringValue(loginname),
              "email" -> ConfigurationValue.StringValue(email),
              "password" -> ConfigurationValue.StringValue(password),
              "status" -> ConfigurationValue.StringValue("active")
            ))
          ))
        ))
      ))
    ))

  private final case class _ProbeUpdateAction(userid: EntityId) extends Action {
    override def name: String = "probeDirectUpdate"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeDirectUpdate",
        properties = List(Property("userAccountId", userid.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeUpdateActionCall(core, this)
  }

  private final case class _ProbeEntityPatchAction(userid: EntityId) extends Action {
    override def name: String = "probeEntityPatch"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeEntityPatch",
        properties = List(Property("userAccountId", userid.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeEntityPatchActionCall(core, this)
  }

  private final case class _ProbeWorkingSetUpsertAction(userid: EntityId) extends Action {
    override def name: String = "probeWorkingSetUpsert"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeWorkingSetUpsert",
        properties = List(Property("userAccountId", userid.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeWorkingSetUpsertActionCall(core, this)
  }

  private final case class _ProbeLoginWorkingSetOnlyAction(
    userid: EntityId
  ) extends Action {
    override def name: String = "probeLoginWorkingSetOnly"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeLoginWorkingSetOnly",
        properties = List(Property("userAccountId", userid.print, None))
      )
    override def createCall(core: ActionCall.Core): ActionCall =
      _ProbeLoginWorkingSetOnlyActionCall(core, this)
  }

  private final case class _ProbeLoginSideEffectsAction(
    userid: EntityId
  ) extends Action {
    override def name: String = "probeLoginSideEffects"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeLoginSideEffects",
        properties = List(Property("userAccountId", userid.print, None))
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
    userid: EntityId
  ) extends Action {
    override def name: String = "probeTypedUserLoad"
    override def request: Request =
      Request.ofService(
        "Probe",
        "probeTypedUserLoad",
        properties = List(Property("userAccountId", userid.print, None))
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
      val cid = executionContext.entityStoreSpace.dataStoreCollection(action.userid).TAKE
      val eid = executionContext.entityStoreSpace.dataStoreEntryId(action.userid).TAKE
      val datastore = executionContext.dataStoreSpace.dataStore(cid).TAKE
      datastore.update(
        cid,
        eid,
        Record.dataAuto("emailVerifiedAt" -> "2026-04-07T00:00:00Z")
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
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("emailVerifiedAt" -> "2026-04-07T00:00:00Z")))
        _ <- entity_update(action.userid, patch)
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
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userid)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userid))
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
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userid)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userid))
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
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("lastLoginAt" -> "2026-04-07T00:00:00Z")))
        _ <- entity_update(action.userid, patch)
        user <- exec_from(
          {
            given ExecutionContext = executionContext
            EntityStore
              .standard()
              .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userid)
          }
            .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userid))
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
      val record = result.records.toVector.map(ComponentFactory.normalizeDataStoreRecord).find(_.getString("email").contains(action.email)).get
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
      val rows = result.records.toVector.map(ComponentFactory.normalizeDataStoreRecord).map { r =>
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
          .load[org.simplemodeling.textus.useraccount.entity.UserAccount](action.userid)
          .flatMap(x => org.goldenport.Consequence.successOrEntityNotFound(x)(action.userid))
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
      val ids = result.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)
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
          case None => org.goldenport.Consequence.entityNotFound(s"user account not found by email: $email")
      yield user

    private def _raw_records(collectionId: org.simplemodeling.model.datatype.EntityCollectionId)(using ExecutionContext): org.goldenport.Consequence[Vector[Record]] =
      for
        cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionId)
        result <- executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty))
      yield result.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)

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
        patch <- exec_from(UserAccountUpdateEntity.createC(Record.dataAuto("lastLoginAt" -> "2026-04-07T00:00:00Z")))
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
          case None => org.goldenport.Consequence.entityNotFound(s"user account not found by email: $email")
      yield user

    private def _raw_credential_by_user_and_password(
      userid: EntityId,
      hashedPassword: String
    )(using ExecutionContext): org.goldenport.Consequence[org.simplemodeling.textus.useraccount.entity.Credential] =
      for
        records <- _raw_records(org.simplemodeling.textus.useraccount.entity.query.Credential.collectionId)
        ids <- records
          .filter(r =>
            r.getString("userAccountId").contains(userid.print) &&
              r.getString("passwordHash").contains(hashedPassword)
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
          case None => org.goldenport.Consequence.securityPermissionDenied("invalid credentials")
      yield credential

    private def _raw_records(collectionId: org.simplemodeling.model.datatype.EntityCollectionId)(using ExecutionContext): org.goldenport.Consequence[Vector[Record]] =
      for
        cid <- executionContext.entityStoreSpace.dataStoreCollection(collectionId)
        result <- executionContext.dataStoreSpace.search(cid, org.goldenport.cncf.datastore.QueryDirective(org.goldenport.cncf.datastore.Query.Empty))
      yield result.records.toVector.map(ComponentFactory.normalizeDataStoreRecord)

    private def _entity_id_of(record: Record): org.goldenport.Consequence[EntityId] =
      record.getAsC[EntityId]("id").flatMap(x => org.goldenport.Consequence.successOrPropertyNotFound("id", x))
  }

}
