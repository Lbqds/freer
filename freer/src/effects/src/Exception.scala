package freer
package effects

import freer.control._

trait Exception[E]
final case class Throw[E <: Throwable](e: E) extends Exception[E]

object Exception {
  def `throw`[U <: Union, E <: Throwable](e: E)(implicit ev: MemberIn[Exception, U]): Freer[U, E] = 
    Freer.send[Exception, U, E](Throw(e))

  def `catch`[U <: Union, E <: Throwable, A](
    fa: Freer[U, A], h: E => Freer[U, A]
  )(implicit ev: MemberIn[Exception, U]): Freer[U, A] = fa match {
    case Effect(_, _) => throw new RuntimeException("internal error")
    case eff @ Pure(_) => eff
    case Bind(fa, f) => fa match {
      case Pure(x) => `catch`[U, E, A](f(x), h)
      case Effect(inner, _) => 
        if (inner.isInstanceOf[Exception[_]]) h(inner.asInstanceOf[Throw[E]].e)
        else fa.flatMap((x: Any) => `catch`[U, E, A](f(x), h))
      case Bind(fa1, f1) => 
        val program1 = fa1.flatMap((x: Any) => f1(x).flatMap(f))
        `catch`[U, E, A](program1, h)
    }
  }

  implicit class ExceptionOps[U <: Union, A](fa: Freer[U, A]) {
    def runExc[E <: Throwable](
      implicit ev1: Exception[_] =:= U#Head[_], 
      ev2: MemberIn[Exception, U]
    ): Freer[U#Tail, Either[E, A]] = {
      Freer.runInterpreter[U, Exception, A, Either[E, A]](
        fa, 
        new Interpreter[Exception, U#Tail, A, Either[E, A]] {
          override def onPure(x: A): Freer[U#Tail,Either[E,A]] = Freer.pure(Right(x))

          override def onEffect[T](eff: Exception[T], cont: T => Freer[U#Tail,Either[E,A]]): Freer[U#Tail,Either[E,A]] =
            Freer.pure(Left(eff.asInstanceOf[Throw[E]].e))
        }
      )
    }
  }
}
