package freer
package util

trait =!=[F1[_], F2[_]]

object NotEq {
  implicit def notEq[F1[_], F2[_]]: =!=[F1, F2] = new =!=[F1, F2] {}

  implicit def eq1[F[_]]: =!=[F, F] = sys.error("type equal")
  implicit def eq2[F[_]]: =!=[F, F] = sys.error("type equal")
}