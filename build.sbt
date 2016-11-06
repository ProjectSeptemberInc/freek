lazy val commonSettings = Seq(
    organization := "com.projectseptember"
  , version := "0.6.5"
  , resolvers ++= Seq(
      Resolver.mavenLocal
    , Resolver.sonatypeRepo("releases")
    , Resolver.sonatypeRepo("snapshots")
    )
  , scalaVersion := "2.11.8"
  , crossScalaVersions := Seq("2.11.8", "2.12.0")
  , bintrayOrganization := Some("projectseptemberinc")
  , licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
  , addCompilerPlugin("org.spire-math" %% "kind-projector"  % "0.9.3" cross CrossVersion.binary)
)

def scalacOptionsVersion(scalaVersion: String) = {
  Seq(
    "-feature"
  , "-language:higherKinds"
  ) ++ (CrossVersion.partialVersion(scalaVersion) match {
         case Some((2, scalaMajor)) if scalaMajor == 12 => Seq("-Ypartial-unification")
         case _ => Seq()
       })
}

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "freek",
    scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
  )
  .settings(
    libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
      deps ++ (
        CrossVersion.partialVersion(ver) match {
          case Some((2, scalaMajor)) if scalaMajor == 11 =>
            Seq(
              "org.typelevel"   %%  "cats-free"  % "0.8.0"
            , "org.scalatest"   %   "scalatest_2.11"      % "3.0.0"             % "test"
            , compilerPlugin("com.milessabin"   % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
            )
          case Some((2, scalaMajor)) if scalaMajor == 12 =>
            Seq(
              "org.typelevel"   %  "cats-free_2.12.0-RC2"  % "0.8.0"
            , "org.scalatest"   %   "scalatest_2.12"       % "3.0.0"             % "test"
            )
        }
      )
    }
  )
