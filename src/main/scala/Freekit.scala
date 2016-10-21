package freek

import scala.reflect.macros.{ blackbox, whitebox }
import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

import cats.free.Free

import scala.language.implicitConversions


class Freekit[DSL0 <: DSL, C0[_] <: CopK[_]](val PRG: DSL.Make[DSL0, C0]) {
  type PRG = PRG.DSL
  type Cop[t] = PRG.Cop[t]

  implicit def liftFA[F[_], A](fa: F[A])( 
    implicit sub0: SubCop[In1[F, ?], Cop]
  ): Free[Cop, A] = {
    Freek.expand[In1[F, ?], Cop, A](Freek(fa))(sub0)
  }

}

class Freekito[DSL0 <: DSL, C0[_] <: CopK[_]](val PRG: DSL.Make[DSL0, C0]) {

  type PRG = PRG.DSL
  type Cop[t] = PRG.Cop[t]
  type O <: Onion

  implicit def liftFGHA[F[_], G[_], HA, A](fga: F[G[HA]])( 
    implicit
      ga: HKK.Aux[G[HA], A]
    , sub0: SubCop[In1[F, ?], PRG.Cop]
    , lifter2: Lifter2.Aux[G[HA], O, A]
    , pointer: Pointer[O]
    , mapper: Mapper[O]
    , binder: Binder[O]
    , traverser: Traverser[O]
  ): OnionT[Free, PRG.Cop, O, A] =
    OnionT.liftTHK(Freek.expand[In1[F, ?], PRG.Cop, G[HA]](Freek(fga))(sub0))

  implicit def liftFA[F[_], A](fa: F[A])( 
    implicit
      sub0: SubCop[In1[F, ?], PRG.Cop]
    , pointer: Pointer[O]
    , mapper: Mapper[O]
    , binder: Binder[O]
    , traverser: Traverser[O]
  ): OnionT[Free, PRG.Cop, O, A] =
    toOnionT0(
      Freek.expand[In1[F, ?], PRG.Cop, A](Freek(fa))(sub0)
    ).onionT[O]

}


