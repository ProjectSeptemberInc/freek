import scala.language.implicitConversions

import cats.{~>, Monad}
import cats.free.Free

/** a few implicit conversions */
package object freek extends HK {

  case class toOnionT3[TC[_[_], _], F[_], GA, A](val tc: TC[F, GA])(
    implicit ga: HKK.Aux[GA, A]
  ) {

    @inline def onionT[S <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter2: Lifter2.Aux[GA, S, A]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[TC, F, S, A] =
      OnionT.liftT3(tc)
  }


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

  /** Will deconstruct G[HA] using HKK & Lifter2 if HA is not simply */
  implicit class ToFreek4[F[_], G[_], HA, A](val fa: F[G[HA]]) extends AnyVal {
    @inline def freek0: Free[In1[F, ?], G[HA]] = Freek(fa)

    @inline def freek[C <: FX](implicit subfx: SubFX[In1[F, ?], C]): Free[subfx.Cop, G[HA]] =
      Freek.expand[In1[F, ?], subfx.Cop, G[HA]](freek0)(subfx.sub) //.asInstanceOf[Free[subfx.Cop, G[HA]]]

    @inline def upcast[T](implicit f: F[G[HA]] <:< T): T = fa
    
    @inline def freeko[C <: FX, S <: Onion](
      implicit 
        ga: HKK.Aux[G[HA], A]
      // , sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
      , subfx: SubFX[In1[F, ?], C]
      , lifter2: Lifter2.Aux[G[HA], S, A]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[Free, subfx.Cop, S, A] = toOnionT3(freek[C]).onionT[S]
  }

  implicit class ToFreek[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def freek0: Free[In1[F, ?], A] = Freek(fa)

    @inline def freek[C <: FX](implicit subfx: SubFX[In1[F, ?], C]): Free[subfx.Cop, A] =
      Freek.expand[In1[F, ?], subfx.Cop, A](freek0)(subfx.sub) //.asInstanceOf[Free[subfx.Cop, A]]

    @inline def upcast[T](implicit f: F[A] <:< T): T = fa
    
    // not working yet... edge-cases make it fail with not cool errors
    @inline def freeko[C <: FX, S <: Onion](
      implicit 
        // sub: SubCop[ConsK[F, CNilK, ?], C#Cop]
        subfx: SubFX[In1[F, ?], C]
      , pointer: Pointer[S]
      , mapper: Mapper[S]
      , binder: Binder[S]
      , traverser: Traverser[S]
    ): OnionT[Free, subfx.Cop, S, A] = toOnionP(freek[C]).onionT[S]
  }

  implicit class FreeExtend[F[_] <: CoproductK[_], A](val free: Free[F, A]) extends AnyVal {
    @inline def expand[C <: FX](implicit subfx: SubFX[F, C]): Free[subfx.Cop, A] =
      Freek.expand[F, subfx.Cop, A](free)(subfx.sub) //.asInstanceOf[Free[subfx.Cop, A]]

    def interpret[F2[_] <: CoproductK[_], G[_]: Monad](i: Interpreter[F2, G])(
      implicit sub:SubCop[F, F2]
    ): G[A] = free.foldMap(new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = i.nat(sub(fa))
    })
  }


  implicit class ToFreeOps[F[_], A](val free: Free[F, A]) extends AnyVal {   
    @inline def expand[C <: FX](implicit subfx: SubFX[In1[F, ?], C]): Free[subfx.Cop, A] =
    free.mapSuspension(new (F ~> In1[F, ?]) {
      def apply[A](fa: F[A]): In1[F, A] = In1(fa)
    }).expand[C]
  }

  implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[In1[F, ?], R] = Interpreter(nat)

  implicit class toOnionT4[C[_]<: CoproductK[_], O <: Onion, A](val onion: OnionT[Free, C, O, A]) extends AnyVal {

    def freeko[F <: FX, O2 <: Onion](
      implicit
        subfxfx: SubFX[C, F]
      , expander: Expander[O, O2]
    ): OnionT[Free, subfxfx.Cop, O2, A] = {
      OnionT(onion.value.expand[F]).expand[O2]
    }
  }
}
