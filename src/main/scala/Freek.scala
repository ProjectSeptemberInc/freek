package freek

// import shapeless.ops.coproduct.{Inject, Selector}
// import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id, DepFn1}

import cats.free.Free
import cats.{Functor, ~>, Monad}

/** Free wrapper using Higher-Kind Coproduct to combine effects F[_] */
/*case class Freek[F[_] <: CoproductK[_], A](free: Free[F, A]) extends AnyVal {

  def flatMap[B](f: A => Freek[F, B]): Freek[F, B] = new Freek(
    free.flatMap { a =>
      f(a).free
    }
  )

  def map[B](f: A => B): Freek[F, B] = new Freek(
    free.map(f)
  )

  def expand[Super[_] <: CoproductK[_]](
    implicit sub: SubCop[F, Super]
  ): Freek[Super, A] = new Freek(free.mapSuspension(
    new (F ~> Super) {
      def apply[A](ga: F[A]): Super[A] = sub(ga)
    }
  ))

  // Funny fact: this is not stacksafe due to recursive call of mapSuspension
  // def flatMap[G[_] <: CoproductK[_], B](f: A => Freek[G, B])(
  //   implicit merge: MergeCopHK[F, G]
  // ): Freek[merge.Out, B] = new Freek(
  //   free.mapSuspension(natF[G]).flatMap { a =>
  //     f(a).free.mapSuspension(natG[G])
  //   }
  // )

  // private def natG[G[_] <: CoproductK[_]](implicit merge: MergeCopHK[F, G]) = new (G ~> merge.Out) {
  //   def apply[A](ga: G[A]): merge.Out[A] = merge.fromRight(ga)
  // }

  // private def natF[G[_] <: CoproductK[_]](implicit merge: MergeCopHK[F, G]) = new (F ~> merge.Out) {
  //   def apply[A](ga: F[A]): merge.Out[A] = merge.fromLeft(ga)
  // } 

}*/

object Freek {

  def apply[F[_], A](fa: F[A]): Free[ConsK[F, CNilK, ?], A] = {
    Free.liftF(Inlk(fa))
  }

  def expand[F[_] <: CoproductK[_], Super[_] <: CoproductK[_], A](free: Free[F, A])(
    implicit sub: SubCop[F, Super]
  ): Free[Super, A] = free.mapSuspension(
    new (F ~> Super) {
      def apply[A](ga: F[A]): Super[A] = sub(ga)
    }
  )
}





