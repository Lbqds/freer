import mill._
import scalalib._

object freer extends ScalaModule {
  val sv = "2.13.5"
  def scalaVersion = sv

  object util extends ScalaModule {
    def scalaVersion = sv
  }

  object control extends ScalaModule {
    def scalaVersion = sv
  }

  object effects extends ScalaModule {
    def scalaVersion = sv
  }

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-mtl:1.2.0",
    ivy"org.typelevel::cats-core:2.3.0"
  )

  def scalacPluginIvyDeps = Agg(
    ivy"org.typelevel:::kind-projector:0.11.3"
  )
}
