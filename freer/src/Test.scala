package freer

import control._
import effects._
import cats.data.StateT

object Test extends App {
  type ET = Amb :|: State :|: UNil

  def program1: Freer[ET, Int] = for {
    b <- Amb.flip[ET]
    _ <- if (b) State.set[ET, Int](3) else State.set[ET, Int](2)
    s <- State.get[ET, Int]
  } yield s

  println(program1.runAmb.runState[Int](0).run)

  // Algebraic Effects for Functional Programming(2.4)
  type _amb[U <: Union] = MemberIn[Amb, U]
  def xor[U <: Union : _amb]: Freer[U, Boolean] = for {
    p <- Amb.flip[U]
    q <- Amb.flip[U]
  } yield (p || q) && !(p && q)

  type _state[U <: Union] = MemberIn[State, U]
  def suprising[U <: Union : _amb : _state]: Freer[U, Boolean] = for {
    p <- Amb.flip[U]
    s <- State.get[U, Int]
    _ <- State.set[U, Int](s + 1)
    b <- if (s >= 1 && p) xor[U] else Freer.pure[U, Boolean](false)
  } yield b

  println(suprising[ET].runAmb.runState[Int](0).run)

  type TE = State :|: Amb :|: UNil
  println(suprising[TE].runState[Int](0).runAmb.run)

  // Effect Handlers in Scope(8)
  type _exc[U <: Union] = MemberIn[Exception, U]
  def decr[U <: Union : _exc : _state]: Freer[U, Unit] = for {
    x <- State.get[U, Int]
    _ <- if (x > 0) State.set[U, Int](x - 1) else Exception.`throw`[U, Throwable](new Throwable("less then 0"))
  } yield ()

  def tripleDecr[U <: Union : _exc : _state]: Freer[U, Unit] = 
    decr[U] >> Exception.`catch`[U, Throwable, Unit](decr[U] >> decr[U], _ => Freer.pure[U, Unit](()))

  type ET1 = State :|: Exception :|: UNil
  println(tripleDecr[ET1].runState[Int](2).runExc.run)

  type TE1 = Exception :|: State :|: UNil
  println(tripleDecr[TE1].runExc.runState[Int](2).run)

  // monad transformer
  import cats.mtl._
  import cats.mtl.implicits._
  import cats._
  import cats.data._
  import cats.implicits._

  object MonadTransformer {
    def decr[F[_]: Monad : Stateful[*[_], Int] : Handle[*[_], Throwable]]: F[Unit] = for {
      x <- Stateful[F, Int].get
      _ <- if (x > 0) Stateful[F, Int].set(x - 1) else Handle[F, Throwable].raise(new Throwable("less then 0"))
    } yield ()

    def tripleDecr[F[_]: Monad : Stateful[*[_], Int] : Handle[*[_], Throwable]]: F[Unit] =
      decr[F] >> Handle[F, Throwable].handleWith(decr[F] >> decr[F])(_ => Monad[F].unit)

  }

  println(MonadTransformer.tripleDecr[StateT[EitherT[Id, Throwable, *], Int, *]].run(2).value)
  println(MonadTransformer.tripleDecr[EitherT[StateT[Id, Int, *], Throwable, *]].value.run(2))
}