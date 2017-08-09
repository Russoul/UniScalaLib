lazy val coreSettings = Seq(
  scalaOrganization := "org.scala-lang",
  scalaVersion  := "2.12.3",
  //scalaVersion := "0.2.0-RC1",
  version := "0.0.1",
  organization := "org.russoul"
)

lazy val uniSettings = Seq(
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
  libraryDependencies += "org.typelevel" %% "spire" % "0.14.1" //using this just for cfor loop
)

lazy val macrosSettings = Seq(
  libraryDependencies+= scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
  libraryDependencies += "org.typelevel" %% "machinist" % "0.6.2"
)

lazy val macrosScalaLib = (project in file("macros")).settings(coreSettings, macrosSettings, name := "MacrosScalaLib")
lazy val uniScalaLib = (project in file(".")).settings(coreSettings, uniSettings, name := "UniScalaLib").dependsOn(macrosScalaLib)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

//libraryDependencies in Scope.GlobalScope += "eu.timepit" %% "singleton-ops" % "0.0.4"
//see machinist project for macros that increase performance of type classes



//scalacOptions += "-Yliteral-types"

//scalacOptions in Runtime ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-Xdisable-assertions")//optimizations
//scalacOptions in Runtime += "-Xplugin-require:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))
scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xdisable-assertions"))
autoCompilerPlugins := true
//addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")




/*



  */

/* // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.patch),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.*/
