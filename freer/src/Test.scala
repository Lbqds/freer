package freer

import control._
import effects._

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
}