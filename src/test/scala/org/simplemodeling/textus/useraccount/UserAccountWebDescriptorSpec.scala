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
  }
}
