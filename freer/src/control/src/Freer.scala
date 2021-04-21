package freer
package control

trait Freer[U <: Union, A] {
  def map[B](f: A => B): Freer[U, B] = Bind(this, (x: A) => Pure(f(x)))
  def flatMap[B](f: A => Freer[U, B]) = Bind(this, f)
  def >>=[B](f: A => Freer[U, B]) = flatMap(f)
  def >>[B](fb: Freer[U, B]): Freer[U, B] = Bind(this, (_: A) => fb)
}

final case class Pure[U <: Union, A](x: A) extends Freer[U, A]
private [freer] final case class Effect[U <: Union, A](inner: Any, index: Int) extends Freer[U, A]
final case class Bind[U <: Union, A, B](fa: Freer[U, A], f: A => Freer[U, B]) extends Freer[U, B]

object Freer {
  def pure[U <: Union, A](x: A): Freer[U, A] = Pure[U, A](x)
  def send[F[_], U <: Union, A](fa: F[A])(implicit ev: MemberIn[F, U]): Freer[U, A] = 
    inject(fa).flatMap(pure)

  private def inject[F[_], U <: Union, A](fa: F[A])(implicit ev: MemberIn[F, U]): Effect[U, A] = Effect(fa, ev.index)
  private [freer] def decompose[F[_], U <: Union, A](eff: Effect[U, A])(implicit ev: MemberIn[F, U]): Either[F[A], Effect[U#Tail, A]] = {
    if (ev.index == 0 && ev.index == eff.index) Left(eff.inner.asInstanceOf[F[A]])
    else Right(Effect[U#Tail, A](eff.inner, eff.index - 1))
  }

  implicit class RunOps[A](p: Freer[UNil, A]) {
    def run: A = p match {
      case Pure(x) => x
      case Bind(fa, f) => new RunOps(f(new RunOps(fa).run)).run
      case _ => throw new RuntimeException("internal error")
    }
  }

  def runInterpreter[U <: Union, F[_], A, B](
    program: Freer[U, A],
    interpreter: Interpreter[F, U#Tail, A, B]
  )(
    implicit ev1: F[_] =:= U#Head[_],
    ev2: MemberIn[F, U]
  ): Freer[U#Tail, B] = program match {
    case Pure(x) => interpreter.onPure(x)
    case _: Effect[U, A] => throw new RuntimeException("internal error")
    case Bind(fa, f) => fa match {
      case Pure(x) => runInterpreter[U, F, A, B](f(x), interpreter)
      case eff @ Effect(inner, _) => decompose[F, U, Any](eff) match {
        case Left(e) => 
          interpreter.onEffect[Any](e, (x: Any) => runInterpreter[U, F, A, B](f(x), interpreter))
        case Right(r) =>
          r.flatMap((x: Any) => runInterpreter[U, F, A, B](f(x), interpreter))
      }
      case Bind(fa1, f1) => 
        val program1 = fa1.flatMap((x: Any) => f1(x).flatMap(f))
        runInterpreter[U, F, A, B](program1, interpreter)
    }
  }
}
