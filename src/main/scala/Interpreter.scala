package freek

import cats.~>

/** helper to combine natural transformations
  * (F ~> R :@: G ~> R:@: H ~> R) gives (F :@: G :@: H :@: CNilK ~> R)
  */
class Interpreter[C[_] <: CoproductK[_], R[_]](
  val nat: C ~> R
) {

  def :@:[F[_]](f: F ~> R): Interpreter[ConsK[F, C, ?], R] = new Interpreter(
    new ~>[ConsK[F, C, ?], R] {
      def apply[A](c: ConsK[F, C, A]): R[A] = c match {
        case Inlk(fa) => f(fa)
        case Inrk(t) => nat(t)
        case _ => throw new RuntimeException("impossible case")
      }
    }
  )

  def :|:[F[_]](f: F ~> R): Interpreter[ConsK[F, C, ?], R] = :@:(f)

}

object Interpreter {
  def apply[F[_], R[_]](nat: F ~> R): Interpreter[ConsK[F, CNilK, ?], R] =
    new Interpreter[ConsK[F, CNilK, ?], R](
      new ~>[ConsK[F, CNilK, ?], R] {
        def apply[A](c: ConsK[F, CNilK, A]): R[A] = c match {
          case Inlk(fa) => nat(fa)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    )
}