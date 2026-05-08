import org.goldenport.cozy.CozyPlugin.autoImport._
import sbt.Keys.*

val scala3Version = "3.3.7"
def sampleVersion(envName: String, fileName: String, fallback: String): String =
  sys.env.get(envName)
    .orElse {
      sys.env.get("CNCF_SAMPLES_ROOT").flatMap { root =>
        val versionFile = file(root) / "versions" / fileName
        if (versionFile.isFile)
          Some(IO.read(versionFile).trim).filter(_.nonEmpty)
        else
          None
      }
    }
    .getOrElse(fallback)

val cncfVersion = sampleVersion("CNCF_VERSION", "cncf-version.conf", "0.4.7-SNAPSHOT")

ThisBuild / organization := "org.textus"
ThisBuild / version := "0.1.1-SNAPSHOT"

lazy val root = (project in file("."))
  .enablePlugins(org.goldenport.cozy.CozyPlugin)
  .settings(
    name := "textus-user-account",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    cozyGeneratorBackend := "cozy",
    cozyDelegateProjectDir := Some(file("/Users/asami/src/dev2025/cozy")),
    resolvers ++= Seq(
      Resolver.defaultLocal,
      Resolver.mavenLocal,
      "SimpleModeling.org" at "https://www.simplemodeling.org/maven"
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
    Test / fork := false
  )
