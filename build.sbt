import sbt.Def.spaceDelimited

name := "advent-of-code-2020"
version := "0.1"
scalaVersion := "2.13.4"
organization := "no.amumurst"
idePackagePrefix.withRank(KeyRanks.Invisible) := Some("no.amumurst")

enablePlugins(GraalVMNativeImagePlugin)
libraryDependencies += "org.graalvm.nativeimage" % "svm" % "20.2.0" % Provided
libraryDependencies += "org.scalameta" %% "svm-subs" % "20.2.0"

lazy val runG = InputKey[Unit]("runG", "Generates Java classes from WSDL")

runG := {
  import sys.process._
  spaceDelimited("<arg>").parsed.headOption match {
    case Some(value) => s"./target/graalvm-native-image/advent-of-code-2020 $value".!
    case None => println("No day given!")
  }
}

addCommandAlias("bg", "graalvm-native-image:packageBin")
addCommandAlias("bgr", "bg; runG")
