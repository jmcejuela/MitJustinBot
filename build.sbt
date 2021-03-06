import sbtassembly.Plugin._
import AssemblyKeys._

/**
 * build configuration (sbt documentation at http://www.scala-sbt.org/)
 */
name := "MitJustinBot"

version := "0.0.1"

organization := "com.jmcejuela.scalatron"

scalaVersion := "2.10.1" //define the exact scala version you want to use, otherwise use system's default

crossPaths := false //disable using the Scala version in output paths and artifacts


/** Configuration */
//sbteclipse: include resources in classpath
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

//sbteclipse: download dependency packages' sources if available
EclipseKeys.withSource := true


/** Publish */
publishTo := Some(Resolver.file("file", new File((Path.userHome / ".m2" / "repository").absolutePath))) //local mvn


/** Repositories */
resolvers ++= Seq(
  "Local Maven Repository" at "file://"+(Path.userHome / ".m2" / "repository").absolutePath)


/** Dependencies */
libraryDependencies ++= Seq(
  /* Test */
  "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
  /* Main */
  //...
)


/** Assembly Plugin (generate jar with all dependencies) */
assemblySettings

test in assembly := {} //do not test while assembling

// Merging Conflicts strategy. See: https://github.com/sbt/sbt-assembly
//
// mergeStrategy in assembly ~= {
// old =>
//  {
//    case PathList("javax", "servlet", _*) => MergeStrategy.first
//    case "application.conf"               => MergeStrategy.filterDistinctLines
//    case "unwanted.txt"                   => MergeStrategy.discard
//    case x                                => old(x)
//  }
// }
