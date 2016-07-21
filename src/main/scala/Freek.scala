package freek

import cats.free.Free
import cats.{Functor, ~>, Monad}


trait Freek[F <: FX, C[_] <: CoproductK[_], A] {
  self =>
  
  def free: Free[C, A]

  def flatMap[B](f: A => Freek[F, C, B]): Freek[F, C, B] = new Freek[F, C, B] {
    def free: Free[C, B] = self.free.flatMap { a => f(a).free.asInstanceOf[Free[C, B]] }
  }

  def map[B](f: A => B): Freek[F, C, B] = new Freek[F, C, B] {
    def free: Free[C, B] = self.free.map(f)
  }

  def expand[F2 <: FX](implicit subfx: SubFX[C, F2]): Freek[F2, subfx.Cop, A] = new Freek[F2, subfx.Cop, A] {
    def free: Free[subfx.Cop, A] = self.free.mapSuspension(
      new (C ~> subfx.Cop) {
        def apply[A](ga: C[A]): subfx.Cop[A] = subfx.sub(ga)
      }
    )
  }
}

/** Just some helpers to enhance Free with CoproductK */
object Freek {

  // def apply[F[_], A](fa: F[A]): Free[ConsK[F, CNilK, ?], A] = {
  //   Free.liftF(Inlk(fa))
  // }

  def apply[F[_], A](fa: F[A]): Free[In1[F, ?], A] = {
    Free.liftF(In1(fa))
  }

  def apply2[F[_], A](fa: F[A]): Freek[F :|: FXNil, In1[F, ?], A] = {
    new Freek[F :|: FXNil, In1[F, ?], A] {
      def free: Free[In1[F, ?], A] = Free.liftF(In1(fa))
    }
  }

  def liftFX[FX0 <: FX, F[_], A](fa: F[A])(implicit subfx: SubFX[In1[F, ?], FX0]): Freek[FX0, subfx.Cop, A] =
    new Freek[FX0, subfx.Cop, A] {
      def free: Free[subfx.Cop, A] = Free.liftF(In1(fa)).mapSuspension(
        new (In1[F, ?] ~> subfx.Cop) {
          def apply[A](ga: In1[F, A]): subfx.Cop[A] = subfx.sub(ga)
        }
      )
    }

  def expand[F[_] <: CoproductK[_], Super[_] <: CoproductK[_], A](free: Free[F, A])(
    implicit sub: SubCop[F, Super]
  ): Free[Super, A] = free.mapSuspension(
    new (F ~> Super) {
      def apply[A](ga: F[A]): Super[A] = sub(ga)
    }
  )

  // def expand2[C0[_] <: CoproductK[_], F <: FX, A](free0: Free[C0, A])(subfx: SubFX[C0, F]): Freek[F, A] =
  //   new Freek[F, A] {
  //     type C[t] = subfx.Cop[t]
  //     def free: Free[C, A] = free0.mapSuspension(
  //       new (C0 ~> C) {
  //         def apply[A](ga: C0[A]): C[A] = subfx.sub(ga)
  //       }
  //     )
  //   }
}
