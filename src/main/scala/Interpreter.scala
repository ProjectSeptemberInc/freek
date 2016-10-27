package freek

import cats.~>

/** helper to combine natural transformations
  * (F ~> R :@: G ~> R:@: H ~> R) gives (F :@: G :@: H :@: CNilK ~> R)
  */
class Interpreter[C[_] <: CopK[_], R[_]](
  val nat: C ~> R
) {

  def :&:[F[_], O[_] <: CopK[_]](f: F ~> R)(implicit prep: PrependHK.Aux[F, C, O]): Interpreter[O, R] = new Interpreter(
    new ~>[O, R] {
      def apply[A](c: O[A]): R[A] = prep.nat(c, f, nat)
    }
  )

  // TBD
  // def :&&:[D[_] <: CopK[_]](f: Interpreter[D])(
  //   implicit merge: MergeCopHK[D, C]
  // ): Interpreter[ConsK[F, C, ?], R] = new Interpreter(
  //   // TBD
  // )

  def :&&:[D[_] <: CopK[_]](f: Interpreter[D, R]): Interpreter[AppendK[C, D, ?], R] = new Interpreter(
    new ~>[AppendK[C, D, ?], R] {
      def apply[A](c: AppendK[C, D, A]): R[A] = c match {
        case Aplk(l) => nat.nat(l)
        case Aprk(r) => f.nat(r)
      }
    }
  )

  def andThen[R2[_]](r2: R ~> R2): Interpreter[C, R2]  = new Interpreter(
    nat andThen r2
  )
}

object Interpreter {
  def apply[F[_], R[_]](nat: F ~> R): Interpreter[In1[F, ?], R] =
    new Interpreter[In1[F, ?], R](
      new ~>[In1[F, ?], R] {
        def apply[A](c: In1[F, A]): R[A] = c match {
          case In1(fa) => nat(fa)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    )
}