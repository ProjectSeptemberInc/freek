import scala.language.implicitConversions

import cats.{~>, Monad}
import cats.free.Free

/** a few implicit conversions */
package object freek extends LowerImplicits with HK {

  /** Will deconstruct G[HA] using HKK & Lifter2 if HA is not simply */
  implicit class ToFreek2[F[_], G[_], HA, A](fa: F[G[HA]])(implicit ga: HKK.Aux[G[HA], A]) {
    @inline def freek0: Free[In1[F, ?], G[HA]] = Freek(fa)

    @inline def freek[C <: DSL](implicit subdsl: SubDSL1[F, C]): Free[subdsl.Cop, G[HA]] =
      Freek.expand[In1[F, ?], subdsl.Cop, G[HA]](freek0)(subdsl.sub)

    @inline def upcast[T](implicit f: F[G[HA]] <:< T): T = fa
    
    @inline def freeko[C <: DSL, O <: Onion](
      implicit 
        subdsl: SubDSL1[F, C]
      , lifter2: Lifter2.Aux[G[HA], O, A]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[Free, subdsl.Cop, O, A] = OnionT.liftTHK(freek[C])
  }

  implicit class ToFreek1[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def freek0: Free[In1[F, ?], A] = Freek(fa)

    @inline def freek[C <: DSL](implicit subdsl: SubDSL1[F, C]): Free[subdsl.Cop, A] =
      Freek.expand[In1[F, ?], subdsl.Cop, A](freek0)(subdsl.sub) //.asInstanceOf[Free[subdsl.Cop, A]]

    @inline def upcast[T](implicit f: F[A] <:< T): T = fa
    
    @inline def freeko[C <: DSL, O <: Onion](
      implicit 
        subdsl: SubDSL1[F, C]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[Free, subdsl.Cop, O, A] = toOnionT0(freek[C]).onionT[O]
  }

  implicit class FreeExtendCopK[F[_] <: CopK[_], A](val free: Free[F, A]) extends AnyVal {
    @inline def expand[C <: DSL](implicit subdsl: SubDSL[F, C]): Free[subdsl.Cop, A] =
      Freek.expand[F, subdsl.Cop, A](free)(subdsl.sub) //.asInstanceOf[Free[subdsl.Cop, A]]

    def interpret[F2[_] <: CopK[_], G[_]: Monad](i: Interpreter[F2, G])(
      implicit sub:SubCop[F, F2]
    ): G[A] = free.foldMap(new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = i.nat(sub(fa))
    })

    def flatten[O[_] <: CopK[_]](implicit flt: Flattener.Aux[F, Free, O]): Free[O, A] = flt.flatten(free)

    def transpile[F2[_] <: CopK[_], G[_] <: CopK[_], O[_] <: CopK[_]](i: Interpreter[F2, G])(
      implicit
        sub:SubCop[F, F2]
      , flt: Flattener.Aux[G, Free, O]
    ): Free[O, A] = flt.flatten(free.compile(new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = i.nat(sub(fa))
    }))

    def transpile[F2[_] <: CopK[_], G[_] <: CopK[_], O[_] <: CopK[_]](i: F2 ~> G)(
      implicit
        sub:SubCop[F, F2]
      , flt: Flattener.Aux[G, Free, O]
    ): Free[O, A] = flt.flatten(free.compile(new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = i(sub(fa))
    }))
  }

  implicit class FreeExtend[F[_], A](val free: Free[F, A]) extends AnyVal {   
    @inline def expand[C <: DSL](implicit subdsl: SubDSL[In1[F, ?], C]): Free[subdsl.Cop, A] =
      free.compile(new (F ~> In1[F, ?]) {
        def apply[A](fa: F[A]): In1[F, A] = In1(fa)
      }).expand[C]
  }

  implicit class toOnionTGA[TC[_[_], _], F[_], GA, A](val tc: TC[F, GA])(
    implicit ga: HKK.Aux[GA, A]
  ) {

    @inline def onionT[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter2: Lifter2.Aux[GA, O, A]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, A] = OnionT.liftTHK(tc)

    @inline def onionT1[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter2: Lifter2.Aux[GA, O, A]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
      , pr: PeelRight[O]
    ): OnionT[TC, F, pr.OutS, pr.Out[A]] = OnionT.liftTHK(tc).peelRight

    @inline def onionT2[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter2: Lifter2.Aux[GA, O, A]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
      , pr2: PeelRight2[O]
    ): OnionT[TC, F, pr2.OutS, pr2.Out[A]] = OnionT.liftTHK(tc).peelRight2

    @inline def onionT3[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter2: Lifter2.Aux[GA, O, A]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
      , pr3: PeelRight3[O]
    ): OnionT[TC, F, pr3.OutS, pr3.Out[A]] = OnionT.liftTHK(tc).peelRight3

    @inline def onion[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, GA] = OnionT.liftP(tc)


    @inline def onionX1[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , liftp: PartialLifter1[GA, O]
      , mapper: Mapper[O]
      , binder: Binder[O]
    ): OnionT[TC, F, O, liftp.GA] = OnionT.liftTPartial1(tc)

    @inline def onionX2[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , liftp: PartialLifter2[GA, O]
      , mapper: Mapper[O]
      , binder: Binder[O]
    ): OnionT[TC, F, O, liftp.GA] = OnionT.liftTPartial2(tc)

  }

  implicit class toOnionExpand[C[_]<: CopK[_], O <: Onion, A](val onion: OnionT[Free, C, O, A]) {

    def freeko[F <: DSL, O2 <: Onion](
      implicit
        subdslDSL: SubDSL[C, F]
      , expander: Expander[O, O2]
    ): OnionT[Free, subdslDSL.Cop, O2, A] = {
      OnionT(onion.value.expand[F]).expand[O2]
    }

  }  
/*
  implicit class toOnionT2[TC[_[_], _], F[_], G[_], H[_], A](val tc: TC[F, G[H[A]]]) extends AnyVal {

    @inline def onionT[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter: Lifter[λ[t => G[H[t]]], O]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, A] =
      OnionT.liftT2(tc)

    @inline def onion[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, G[H[A]]] = OnionT.liftP(tc)

  }

  implicit class toOnionT1[TC[_[_], _], F[_], G[_], A](val tc: TC[F, G[A]]) extends AnyVal {

    @inline def onionT[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , lifter: Lifter[G, O]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, A] = OnionT.liftT(tc)

    @inline def onion[O <: Onion](
      implicit 
        tcMonad: Monad[TC[F, ?]]
      , pointer: Pointer[O]
      , mapper: Mapper[O]
      , binder: Binder[O]
      , traverser: Traverser[O]
    ): OnionT[TC, F, O, G[A]] = OnionT.liftP(tc)

  }
*/

}

package freek {
  trait LowerImplicits extends LowerImplicits2 {
    implicit class toOnionT0[TC[_[_], _], F[_], A](val tc: TC[F, A]) {

      @inline def onionT[O <: Onion](
      implicit 
          tcMonad: Monad[TC[F, ?]]
        , pointer: Pointer[O]
        , mapper: Mapper[O]
        , binder: Binder[O]
        , traverser: Traverser[O]
      ): OnionT[TC, F, O, A] =
        OnionT.liftP(tc)

    }

    implicit def toInterpreterCopK[F[_] <: CopK[_], R[_]](nat: F ~> R): Interpreter[F, R] = new Interpreter(nat)

  }

  trait LowerImplicits2 {
    implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[In1[F, ?], R] = Interpreter(nat)
  }
}