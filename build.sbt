organization := "ni-apr"

name := "microc"

version := "0.1"

scalaVersion := "2.13.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

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

