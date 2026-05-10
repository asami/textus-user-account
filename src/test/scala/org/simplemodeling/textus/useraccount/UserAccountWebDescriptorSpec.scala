package org.simplemodeling.textus.useraccount

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class UserAccountWebDescriptorSpec extends AnyWordSpec with Matchers {
  "User account Web descriptor" should {
    "declare authentication pages as screen-mode pages" in {
      val webYaml = Files.readString(
        Paths.get("src/main/car/web/web.yaml"),
        StandardCharsets.UTF_8
      )

      Vector(
        "signin",
        "signup",
        "password-reset",
        "password-reset-sent",
        "password-reset-confirm",
        "password-reset-success",
        "two-factor",
        "two-factor-challenge"
      ).foreach { page =>
        webYaml should include (s"  ${page}:\n    mode: screen")
      }
    }

    "read debug-wrapped form API session responses on sign-in pages" in {
      val signin = Files.readString(
        Paths.get("src/main/web/signin/index.html"),
        StandardCharsets.UTF_8
      )
      val challenge = Files.readString(
        Paths.get("src/main/web/two-factor-challenge/index.html"),
        StandardCharsets.UTF_8
      )

      signin should include ("function responseDataOf(payload)")
      signin should include ("payload.data")
      signin should include ("function accessSessionIdOf(payload)")
      signin should include ("function twoFactorRequiredOf(payload)")
      signin should include ("twoFactorRequiredOf(payload)")
      challenge should include ("function responseDataOf(payload)")
      challenge should include ("payload.data")
      challenge should include ("function accessSessionIdOf(payload)")
    }
  }
}
