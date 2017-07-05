scalaOrganization := "org.typelevel"
scalaVersion := "2.11.11-bin-typelevel-4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies in Scope.GlobalScope += scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"
libraryDependencies in Scope.GlobalScope += scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
libraryDependencies in Scope.GlobalScope += "org.typelevel" %% "cats" % "0.9.0"
libraryDependencies in Scope.GlobalScope += "org.typelevel" %% "kittens" % "1.0.0-M9" //auto typeclass derivation
libraryDependencies in Scope.GlobalScope += "org.typelevel" %% "spire" % "0.14.1" //algebra and numeric computing
libraryDependencies in Scope.GlobalScope += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies in Scope.GlobalScope += "eu.timepit" %% "singleton-ops" % "0.0.4"

scalacOptions += "-Yliteral-types"

scalacOptions in Runtime ++= Seq("-optimise", "-Yclosure-elim", "-Yinline", "-Xdisable-assertions")//optimizations
scalacOptions in Runtime += "-Xplugin-require:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))
scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"
scalacOptions in Test ~= (_ filterNot (_ == "-Xdisable-assertions"))
autoCompilerPlugins := true
addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")






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
