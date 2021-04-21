package freer
package effects

import freer.control._

trait Amb[T]
final case object Flip extends Amb[Boolean]

object Amb {
  def flip[U <: Union](implicit ev: MemberIn[Amb, U]): Freer[U, Boolean] = 
    Freer.send[Amb, U, Boolean](Flip)

  implicit class AmbOps[U <: Union, A](fa: Freer[U, A]) {
    def runAmb(implicit ev1: Amb[_] =:= U#Head[_], ev2: MemberIn[Amb, U]): Freer[U#Tail, List[A]] = {
      Freer.runInterpreter[U, Amb, A, List[A]](fa, new Interpreter[Amb, U#Tail, A, List[A]] {
        override def onPure(x: A): Freer[U#Tail,List[A]] = Freer.pure[U#Tail, List[A]](List(x))

        override def onEffect[T](eff: Amb[T], cont: T => Freer[U#Tail,List[A]]): Freer[U#Tail,List[A]] = {
          val f = cont.asInstanceOf[Boolean => Freer[U#Tail, List[A]]]
          for {
            left  <- f(false)
            right <- f(true)
          } yield left ++ right
        }
      })
    }
  }
}
