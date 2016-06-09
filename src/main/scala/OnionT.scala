package freek

import cats.free.Free
import cats.{Applicative, Functor, FlatMap, Monad, Traverse, Eq}


/** The OnionT transformer to manipulate monadic stack of results */
case class OnionT[TC[_[_], _], F[_], S <: Onion, A](value: TC[F, S#Build[A]])(
  implicit
    tcMonad: Monad[TC[F, ?]]
  , mapper: Mapper[S]
  , binder: Binder[S]
  , traverser: Traverser[S]
) extends Product with Serializable {

  def map[B](f: A => B): OnionT[TC, F, S, B] =
    OnionT(tcMonad.map(value){ a => mapper.map(a)(f) })

  def flatMap[B](f: A => OnionT[TC, F, S, B]): OnionT[TC, F, S, B] =
    OnionT(
      tcMonad.flatMap(value){ sba: S#Build[A] =>
        val subsbb = traverser.traverse(sba){ a => f(a).value }
        tcMonad.map(subsbb) { sbb => binder.bind(sbb){ sb => sb } }
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

trait OnionTHelpers extends OnionTHelpersLow {

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

  }

}

trait OnionTHelpersLow {

  implicit class toOnionP[TC[_[_], _], F[_], G[_], A](tc: TC[F, A]) {

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

}