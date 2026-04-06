package org.simplemodeling.textus.useraccount
import org.goldenport.cncf.context.{ExecutionContext, SecurityContext}
import org.goldenport.cncf.directive.Query
import org.goldenport.datatype.Name
import org.simplemodeling.model.datatype.EntityId
import org.simplemodeling.model.directive.Condition
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/*
 * @since   Apr.  6, 2026
 * @version Apr.  6, 2026
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
  }
}
