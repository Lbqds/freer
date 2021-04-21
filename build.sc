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
}
