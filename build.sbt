name := "scalametric"

version := "1.0"

lazy val `scalametric` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.5"

libraryDependencies ++= Seq( jdbc ,
                             cache ,
                             ws ,
                             specs2 % Test ,
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.typelevel" %% "cats" % "0.4.1",
  "org.spire-math" %% "spire" % "0.11.0"
          )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

