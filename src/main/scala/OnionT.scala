package freek

import cats.free.Free
import cats.{Applicative, Functor, FlatMap, Monad, Traverse, Eq}


/** The OnionT transformer to manipulate monadic stack of results */
case class OnionT[TC[_[_], _], F[_], S <: Onion, A](value: TC[F, S#Build[A]]) extends Product with Serializable {

  def map[B](f: A => B)(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , mapper: Mapper[S]
  ): OnionT[TC, F, S, B] =
    OnionT(tcMonad.map(value){ a => mapper.map(a)(f) })

  def flatMap[B](f: A => OnionT[TC, F, S, B])(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, B] =
    OnionT(
      tcMonad.flatMap(value){ sba: S#Build[A] =>
        val subsbb = traverser.traverse(sba){ a => f(a).value }
        tcMonad.map(subsbb) { sbb => binder.bind(sbb){ sb => sb } }
      }
    )

  def downRight(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , dr: DownRight[S]
  ): OnionT[TC, F, dr.OutS, dr.Out[A]] =
    OnionT[TC, F, dr.OutS, dr.Out[A]](
      tcMonad.map(value){ sba: S#Build[A] =>
        dr.downRight(sba)
      }
    )

  def upLeft[H[_]](
    implicit
      tcMonad: Monad[TC[F, ?]]
    , ul: UpLeft[H, S]
  ): OnionT[TC, F, ul.Out, A] =
    OnionT[TC, F, ul.Out, A](
      tcMonad.map(value){ sba: S#Build[A] =>
        ul.upLeft(sba)
      }
    )

  def expand[S2 <: Onion](
    implicit
      tcMonad: Monad[TC[F, ?]]
    , expander: Expander[S, S2]
  ): OnionT[TC, F, S2, A] =
    OnionT(
      tcMonad.map(value){ sba: S#Build[A] =>
        expander.expand(sba)
      }
    )

}


object OnionT extends OnionTInstances {

  def pure[TC[_[_], _], F[_], S <: Onion, A](a: A)(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , pointer: Pointer[S]
    , mapper: Mapper[S]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, A] =
    liftP(tcMonad.pure(a))

  def liftF[TC[_[_], _], F[_], S <: Onion, G[_], A](fa: G[A])(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , lifter: Lifter[G, S]
    , mapper: Mapper[S]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, A] =
    liftT(tcMonad.pure(fa))

  def liftP[TC[_[_], _], F[_], S <: Onion, A](fa: TC[F, A])(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , pointer: Pointer[S]
    , mapper: Mapper[S]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, A] =
    OnionT(tcMonad.map(fa) (a => pointer.point(a)))

  def liftT[TC[_[_], _], F[_], S <: Onion, G[_], A](fa: TC[F, G[A]])(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , lifter: Lifter[G, S]
    , mapper: Mapper[S]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, A] =
    OnionT(tcMonad.map(fa){ fa => lifter.lift(fa) })

  def liftT2[TC[_[_], _], F[_], S <: Onion, G[_], H[_], A](fa: TC[F, G[H[A]]])(
    implicit
      tcMonad: Monad[TC[F, ?]]
    , lifter: Lifter[λ[t => G[H[t]]], S]
    , mapper: Mapper[S]
    , binder: Binder[S]
    , traverser: Traverser[S]
  ): OnionT[TC, F, S, A] =
    OnionT(tcMonad.map(fa){ fa => lifter.lift(fa) })

  // def liftT3[TC[_[_], _], F[_], S <: Onion, GA](fa: TC[F, GA])(
  //   implicit
  //     tcMonad: Monad[TC[F, ?]]
  //   , lifter2: Lifter2[GA, S]
  //   , mapper: Mapper[S]
  //   , binder: Binder[S]
  //   , traverser: Traverser[S]
  // ): OnionT[TC, F, S, lifter2.A] =
  //   OnionT(tcMonad.map(fa){ fa => lifter2.lift2(fa) })
}

trait OnionTInstances {

  implicit def monad[TC[_[_], _], F[_], S <: Onion](
    implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
  ): Monad[OnionT[TC, F, S, ?]] = new Monad[OnionT[TC, F, S, ?]] {
    def pure[A](a: A) = OnionT(tcMonad.pure(pointer.point(a)))

    def flatMap[A, B](fa: OnionT[TC, F, S, A])(f: A => OnionT[TC, F, S, B]): OnionT[TC, F, S, B] =
      fa.flatMap(f)

    override def map[A, B](fa: OnionT[TC, F, S, A])(f: A => B): OnionT[TC, F, S, B] =
      fa.map(f)
  }


}

trait OnionTHelpers {

  implicit class toOnionT2[TC[_[_], _], F[_], G[_], H[_], A](tc: TC[F, G[H[A]]]) {

    def onionT[S <: Onion](
    implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter: Lifter[λ[t => G[H[t]]], S]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftT2(tc)

  }

  implicit class toOnionT[TC[_[_], _], F[_], G[_], A](tc: TC[F, G[A]]) {

    def onionT[S <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter: Lifter[G, S]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftT(tc)

    def onionP[S <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ) = OnionT.liftP(tc)

  }

  implicit class toOnionP[TC[_[_], _], F[_], A](tc: TC[F, A]) {

    def onionT[S <: Onion](
    implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftP(tc)

  }

  // case class toOnionT3[TC[_[_], _], F[_], GA](tc: TC[F, GA]) {

  //   def onionT[S <: Onion](
  //   implicit 
  //       tcMonad: Monad[TC[F, ?]]
  //     , lifter2: Lifter2[GA, S]
  //     , pointer: Pointer[S]
  //     , mapper: Mapper[S]
  //     , binder: Binder[S]
  //     , traverser: Traverser[S]
  //   ): OnionT[TC, F, S, lifter2.A] =
  //     OnionT.liftT3(tc)

  // }
}
