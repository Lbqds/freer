package freer
package effects

import freer.control._

trait State[+S]
final case class Set[S](value: S) extends State[Unit]
final case class Get[S]() extends State[S]

object State {
  def set[U <: Union, S](init: S)(implicit ev: MemberIn[State, U]): Freer[U, Unit] = 
    Freer.send[State, U, Unit](Set(init))

  def get[U <: Union, S](implicit ev: MemberIn[State, U]): Freer[U, S] = 
    Freer.send[State, U, S](Get[S])

  implicit class StateOps[U <: Union, A](fa: Freer[U, A]) {
    def runState[S](init: S)(implicit ev1: State[_] =:= U#Head[_], ev2: MemberIn[State, U]): Freer[U#Tail, (A, S)] = fa match {
      case Pure(x) => Freer.pure[U#Tail, (A, S)]((x, init))
      case Effect(_, _) => throw new RuntimeException("internal error")
      case Bind(fa, f) => fa match {
        case Pure(x) => new StateOps(f(x)).runState(init)
        case eff @ Effect(_, _) => Freer.decompose[State, U, Any](eff) match {
          case Left(e) => e match {
            case Get() => new StateOps(f(init)).runState(init)
            case Set(value) => new StateOps(f()).runState(value.asInstanceOf[S])
          }
          case Right(r) => r.flatMap((x: Any) => new StateOps(f(x)).runState(init))
        }
        case Bind(fa1, f1) => 
          val program1 = fa1.flatMap((x: Any) => f1(x).flatMap(f))
          new StateOps(program1).runState(init)
      }
    }
  }
}