import scala.language.implicitConversions

import cats.{~>, Monad}
import cats.free.Free

/** a few implicit conversions */
package object freek extends HK {

  // case class toOnionT3[TC[_[_], _], F[_], GA, Out[_], A](val tc: TC[F, GA])(
  //   implicit ga: HKK.Aux[GA, Out, A]
  // ) {

  //   @inline def onionT[S <: Onion](
  //     implicit 
  //       tcMonad: Monad[TC[F, ?]]
  //     , lifter2: Lifter2.Aux[GA, S, A]
  //     , pointer: Pointer[S]
  //     , mapper: Mapper[S]
  //     , binder: Binder[S]
  //     , traverser: Traverser[S]
  //   ): OnionT[TC, F, S, A] =
  //     OnionT.liftT3(tc)
  // }

  implicit class toOnionT2[TC[_[_], _], F[_], G[_], H[_], A](val tc: TC[F, G[H[A]]]) extends AnyVal {

    @inline def onionT[S <: Onion](
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

  implicit class toOnionT[TC[_[_], _], F[_], G[_], A](val tc: TC[F, G[A]]) extends AnyVal {

    @inline def onionT[S <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter: Lifter[G, S]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftT(tc)

    @inline def onionP[S <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ) = OnionT.liftP(tc)

  }

  implicit class toOnionP[TC[_[_], _], F[_], A](val tc: TC[F, A]) extends AnyVal {

    @inline def onionT[S <: Onion](
    implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftP(tc)

  }

  // implicit class ToFreek4[F[_], GHA, Out[_], A](val fa: F[GHA])(implicit ga: HKK.Aux[GHA, Out, A]) {
  //   @inline def freek0: Free[ConsK[F, CNilK, ?], GHA] = Freek(fa)

  //   @inline def freek[C <: FX](implicit sub: SubCop[ConsK[F, CNilK, ?], C#Cop]): Free[C#Cop, GHA] =
  //     Freek.expand[ConsK[F, CNilK, ?], C#Cop, GHA](freek0)

  //   @inline def upcast[T](implicit f: F[GHA] <:< T): T = fa
    
  //   @inline def freeko[C <: FX, S <: Onion](
  //     implicit 
  //       sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
  //     , lifter: Lifter2.Aux[GHA, S, A]
  //     , pointer: Pointer[S]
  //     , mapper: Mapper[S]
  //     , binder: Binder[S]
  //     , traverser: Traverser[S]
  //   ) = toOnionT3(freek[C]).onionT[S]
  // }

  implicit class ToFreek3[F[_], G[_], H[_], A](val fa: F[G[H[A]]]) extends AnyVal {
    @inline def freek0: Free[ConsK[F, CNilK, ?], G[H[A]]] = Freek(fa)

    @inline def freek[C <: FX](implicit sub: SubCop[ConsK[F, CNilK, ?], C#Cop]): Free[C#Cop, G[H[A]]] =
      Freek.expand[ConsK[F, CNilK, ?], C#Cop, G[H[A]]](freek0)

    @inline def upcast[T](implicit f: F[G[H[A]]] <:< T): T = fa
    
    @inline def freeko[C <: FX, S <: Onion](
      implicit 
        sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
      , lifter: Lifter[λ[t => G[H[t]]], S]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ) = toOnionT2(freek[C]).onionT[S]
  }

  implicit class ToFreek2[F[_], G[_], A](val fa: F[G[A]]) extends AnyVal {
    @inline def freek0: Free[ConsK[F, CNilK, ?], G[A]] = Freek(fa)

    @inline def freek[C <: FX](implicit sub: SubCop[ConsK[F, CNilK, ?], C#Cop]): Free[C#Cop, G[A]] =
      Freek.expand[ConsK[F, CNilK, ?], C#Cop, G[A]](freek0)

    @inline def upcast[T](implicit f: F[G[A]] <:< T): T = fa
    
    @inline def freeko[C <: FX, S <: Onion](
      implicit 
        sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
      , lifter: Lifter[G, S]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ) = toOnionT(freek[C]).onionT[S]
  }

  implicit class ToFreek[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def freek0: Free[ConsK[F, CNilK, ?], A] = Freek(fa)

    @inline def freek[C <: FX](implicit sub: SubCop[ConsK[F, CNilK, ?], C#Cop]): Free[C#Cop, A] =
      Freek.expand[ConsK[F, CNilK, ?], C#Cop, A](freek0)

    @inline def upcast[T](implicit f: F[A] <:< T): T = fa
    
    // not working yet... edge-cases make it fail with not cool errors
    @inline def freeko[C <: FX, S <: Onion](
      implicit 
        sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ) = toOnionP(freek[C]).onionT[S]
  }

  implicit class FreeExtend[F[_] <: CoproductK[_], A](val free: Free[F, A]) extends AnyVal {
    @inline def expand[C <: FX](implicit sub: SubCop[F, C#Cop]): Free[C#Cop, A] =
      Freek.expand[F, C#Cop, A](free)

    def interpret[F2[_] <: CoproductK[_], G[_]: Monad](i: Interpreter[F2, G])(
      implicit sub:SubCop[F, F2]
    ): G[A] = free.foldMap(new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = i.nat(sub(fa))
    })
  }

  implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[ConsK[F, CNilK, ?], R] = Interpreter(nat)

}
