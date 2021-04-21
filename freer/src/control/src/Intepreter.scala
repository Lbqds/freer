package freer
package control

trait Interpreter[F[_], U <: Union, A, B] {
  def onPure(x: A): Freer[U, B]
  def onEffect[T](eff: F[T], cont: T => Freer[U, B]): Freer[U, B]
}
