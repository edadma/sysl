name := "-scala-native"

version := "0.1.0"

scalaVersion := "2.11.12"

enablePlugins(ScalaNativePlugin)

nativeLinkStubs := true

nativeMode := "debug"

nativeLinkingOptions := Seq( s"-L/${baseDirectory.value}/native-lib" )

scalacOptions ++= Seq( "-deprecation", "-feature", "-unchecked", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".Main" )

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.1" % "test"

testFrameworks += new TestFramework( "utest.runner.Framework" )

libraryDependencies ++= Seq(
  "com.github.scopt" %%% "scopt" % "3.7.1"
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
