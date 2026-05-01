import org.goldenport.cozy.CozyPlugin.autoImport._

ThisBuild / organization := "org.textus"
ThisBuild / version := "0.1.1-SNAPSHOT"

lazy val root = (project in file("."))
  .enablePlugins(org.goldenport.cozy.CozyPlugin)
  .settings(
    name := "textus-user-account",
    scalaVersion := "3.3.7",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    cozyGeneratorBackend := "cozy",
    cozyDelegateProjectDir := Some(file("/Users/asami/src/dev2025/cozy")),
    resolvers ++= Seq(
      Resolver.defaultLocal,
      Resolver.mavenLocal,
      "SimpleModeling.org" at "https://www.simplemodeling.org/maven"
    ),
    libraryDependencies ++= Seq(
      "org.goldenport" %% "goldenport-cncf" % "0.4.6-SNAPSHOT",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    cozyManifestMetadata ++= Map(
      "component" -> "textus-user-account",
      "boundedContext" -> "identity",
      "domain" -> "user-account"
    ),
    Test / fork := false
  )
