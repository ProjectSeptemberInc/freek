package freek

import cats.free.Free
import cats.{Functor, ~>, Monad}


/** Just some helpers to enhance Free with CopK */
object Freek {

  def apply[F[_], A](fa: F[A]): Free[In1[F, ?], A] = {
    Free.liftF(In1(fa))
  }

  def expand[F[_] <: CopK[_], Super[_] <: CopK[_], A](free: Free[F, A])(
    implicit sub: SubCop[F, Super]
  ): Free[Super, A] = free.mapSuspension(
    new (F ~> Super) {
      def apply[A](ga: F[A]): Super[A] = sub(ga)
    }
  )

}
