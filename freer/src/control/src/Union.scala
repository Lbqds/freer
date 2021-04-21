package freer
package control

import freer.util._

trait Union {
  type Head[_]
  type Tail <: Union
}

trait :|:[F[_], U <: Union] extends Union {
  type Head[_] = F[_]
  type Tail = U
}

trait UNil extends Union {
  type Head[_] = Nothing
  type Tail = Nothing
}

trait MemberIn[F[_], U <: Union] {
  val index: Int
}

object MemberIn {
  implicit def memberIn0[F[_], U <: Union]: MemberIn[F, F :|: U] = new MemberIn[F, F :|: U] {
    val index = 0
  }

  implicit def memberIn1[F[_], U <: Union](implicit m: MemberIn[F, U#Tail]): MemberIn[F, U] = new MemberIn[F, U] {
    val index = m.index + 1
  }

  implicit def memberNotIn0[F[_]]: MemberIn[F, UNil] = sys.error("effect type isn't a member of union")
  implicit def memberNotIn1[F[_]]: MemberIn[F, UNil] = sys.error("effect type isn't a member of union")
}
