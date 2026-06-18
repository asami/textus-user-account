import org.goldenport.cozy.CozyPlugin.autoImport._
import sbt.Keys.*

val scala3Version = "3.3.7"
val cncfVersion = "0.4.11"

ThisBuild / organization := "org.textus"
ThisBuild / version := "0.1.4"

lazy val root = (project in file("."))
  .enablePlugins(org.goldenport.cozy.CozyPlugin)
  .settings(
    name := "textus-user-account",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    cozyGeneratorBackend := "cozy",
    cozyGenerationVersionOverrides ++= Map(
      "generation.versions.cncf" -> cncfVersion
    ),
    resolvers ++= Seq(
      Resolver.defaultLocal,
      Resolver.mavenLocal,
      "SimpleModeling.org" at "https://www.simplemodeling.org/repository/maven"
    ),
    libraryDependencies ++= Seq(
      "org.goldenport" %% "goldenport-cncf" % cncfVersion,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    cozyManifestMetadata ++= Map(
      "component" -> "textus-user-account",
      "boundedContext" -> "identity",
      "domain" -> "user-account"
    ),
    publish := {
      val _ = cozyPublishCar.value
      ()
    },
    publishLocal := {
      val _ = cozyPublishLocalCar.value
      ()
    },
    versionScheme := Some("early-semver"),
    publishMavenStyle := true,
    publishTo := {
      val repo = sys.env.get("SIMPLEMODELING_MAVEN_LOCAL")
        .map(file)
        .getOrElse(baseDirectory.value / "maven-local")

      Some(
        Resolver.file(
          "local-simplemodeling-maven",
          repo
        )
      )
    },
    Test / fork := false
  )
