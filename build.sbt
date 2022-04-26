organization := "ni-apr"

name := "microc"

version := "0.1"

scalaVersion := "2.13.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Werror", "-Ymacro-annotations")

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "kittens" % "2.3.2"
libraryDependencies += "org.jetbrains" % "annotations" % "23.0.0"
libraryDependencies += "com.chuusai" % "shapeless_2.13" % "2.4.0-M1"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

testFrameworks += new TestFramework("munit.Framework")

// --------------------------------------------------------------------
// ASSEMBLY
// --------------------------------------------------------------------
assembly / assemblyJarName := "microc.jar"

// --------------------------------------------------------------------
// DOCKER
// --------------------------------------------------------------------
enablePlugins(DockerPlugin)
docker / dockerfile := {
  val artifact: File = assembly.value
  val artifactTargetPath = "/" + artifact.name

  new Dockerfile {
    from("openjdk:11-jre")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}

// create both latest and current version
val imageOrgName = s"fikovnik"
docker / imageNames := Seq(
  ImageName(s"$imageOrgName/${name.value}:latest"),
  ImageName(s"$imageOrgName/${name.value}:${version.value}")
)

// --------------------------------------------------------------------
// JMH
// --------------------------------------------------------------------
enablePlugins(JmhPlugin)

// allow benchmarks in tests (cf. https://github.com/sbt/sbt-jmh)
import pl.project13.scala.sbt.JmhPlugin.JmhKeys.Jmh
Jmh / sourceDirectory := (Test / sourceDirectory).value
Jmh / classDirectory := (Test / classDirectory).value
Jmh / dependencyClasspath := (Test / dependencyClasspath).value
Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value
Jmh / run := (Jmh / run).dependsOn(Jmh / Keys.compile).evaluated

