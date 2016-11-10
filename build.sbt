lazy val commonSettings = Seq(
    organization := "com.projectseptember"
  , version := "0.6.5"
  , resolvers ++= Seq(
      Resolver.mavenLocal
    , Resolver.sonatypeRepo("releases")
    , Resolver.sonatypeRepo("snapshots"))
  , scalaVersion := "2.11.8"
  , crossScalaVersions := Seq("2.11.8", "2.12.0")
  , bintrayOrganization := Some("projectseptemberinc")
  , licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
  , addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
)

def scalacOptionsVersion(scalaVersion: String) = {
  Seq(
    "-feature"
  , "-language:higherKinds"
  ) ++ (CrossVersion.partialVersion(scalaVersion) match {
         case Some((2, scalaMajor)) if scalaMajor == 12 => Seq("-Ypartial-unification")
         case _ => Nil
       })
}

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "freek",
    scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
  )
  .settings(
    libraryDependencies ++= Seq(
        "org.typelevel"   %%  "cats-free"  % "0.8.1"
      , "org.scalatest"   %%  "scalatest"  % "3.0.0"  % Test
    ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor == 11 =>
        compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full) :: Nil
      case _ => Nil
    })
  )
