package freek

// import shapeless.ops.coproduct.{Inject, Selector}
// import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id, DepFn1}

import cats.free.Free
import cats.{Functor, ~>, Monad}

/** Free wrapper using Higher-Kind Coproduct to combine effects F[_] */
class Freek[F[_] <: CoproductK[_], A](val free: Free[F, A]) {

  def flatMap[G[_] <: CoproductK[_], B](f: A => Freek[G, B])(
    implicit merge: MergeCopHK[F, G]
  ): Freek[merge.Out, B] = new Freek(
    free.mapSuspension(natF[G]).flatMap { a =>
      f(a).free.mapSuspension(natG[G])
    }
  )

  def map[B](f: A => B): Freek[F, B] = new Freek(
    free.map(f)
  )

  private def natG[G[_] <: CoproductK[_]](implicit merge: MergeCopHK[F, G]) = new (G ~> merge.Out) {
    def apply[A](ga: G[A]): merge.Out[A] = merge.fromRight(ga)
  }

  private def natF[G[_] <: CoproductK[_]](implicit merge: MergeCopHK[F, G]) = new (F ~> merge.Out) {
    def apply[A](ga: F[A]): merge.Out[A] = merge.fromLeft(ga)
  } 

}

object Freek {

  def apply[F[_], A](fa: F[A]): Freek[ConsK[F, CNilK, ?], A] = {
    new Freek[ConsK[F, CNilK, ?], A](
      Free.liftF(Inlk(fa))
    )
  }

}





