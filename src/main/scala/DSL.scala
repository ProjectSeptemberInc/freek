package freek

/** a type helper to build Coproduct of effects F[_] with a clean syntax
  *
  * NoDSL is equivalent to higher-kinded CNil
  * to build the equivalent of [t => F[t] :+: G[t] :+: CNilk[t]], use following syntax
  *
  */
sealed trait DSL

final class :|:[H[_], T <: DSL] extends DSL

final class :||:[T1 <: DSL, T2 <: DSL] extends DSL

trait NoDSL extends DSL
case object NoDSL extends NoDSL

object DSL {
  class Make[DSL0 <: DSL, C[_] <: CopK[_]] {
    type Cop[t] = C[t]
    type DSL = DSL0
  }

  object Make {
    def apply[DSL0 <: DSL](implicit toCop: ToCopK[DSL0]) = new Make[DSL0, toCop.Cop] {}
  }
}

trait ToCopK[F <: DSL] {
  type Cop[_] <: CopK[_]
}

object ToCopK extends LowerToCopK {

  def apply[F <: DSL](implicit toCopK: ToCopK[F]) = toCopK

  type Aux[F <: DSL, C[_] <: CopK[_]] = ToCopK[F] {
    type Cop[t] = C[t]
  }

  implicit val NoDSL: ToCopK.Aux[NoDSL, CNilK] = new ToCopK[NoDSL] {
    type Cop[t] = CNilK[t]
  }

  implicit def one[H[_]]: ToCopK.Aux[:|:[H, NoDSL], In1[H, ?]] =
    new ToCopK[:|:[H, NoDSL]] {
      type Cop[t] = In1[H, t]
    }

  implicit def two[H[_], H2[_]]: ToCopK.Aux[:|:[H, :|:[H2, NoDSL]], In2[H, H2, ?]] =
    new ToCopK[:|:[H, :|:[H2, NoDSL]]] {
      type Cop[t] = In2[H, H2, t]
    }

  implicit def three[H[_], H2[_], H3[_]]: ToCopK.Aux[:|:[H, :|:[H2, :|:[H3, NoDSL]]], In3[H, H2, H3, ?]] =
    new ToCopK[:|:[H, :|:[H2, :|:[H3, NoDSL]]]] {
      type Cop[t] = In3[H, H2, H3, t]
    }

}

trait LowerToCopK {

  implicit def rec[H[_], T <: DSL, C[_] <: CopK[_], O[_] <: CopK[_]](
    implicit
      next: ToCopK.Aux[T, C]
    , prep: PrependHK.Aux[H, C, O]
  ): ToCopK.Aux[:|:[H, T], O] =
    new ToCopK[:|:[H, T]] {
      type Cop[t] = O[t]
    }

  implicit def merge[T1 <: DSL, T2 <: DSL, C1[_] <: CopK[_], C2[_] <: CopK[_]](
    implicit
      toCopK1: ToCopK.Aux[T1, C1]
    , toCopK2: ToCopK.Aux[T2, C2]
  ): ToCopK.Aux[:||:[T1, T2], AppendK[C1, C2, ?]] =
    new ToCopK[:||:[T1, T2]] {
      type Cop[t] = AppendK[C1, C2, t]
    }

}

trait SubDSL[C[_] <: CopK[_], F <: DSL] {
  type Cop[_] <: CopK[_]

  val sub: SubCop[C, Cop]
}

object SubDSL {

  def apply[C[_] <: CopK[_], F <: DSL](implicit subDSLDSL: SubDSL[C, F]) = subDSLDSL

  implicit def subDSL[C[_] <: CopK[_], F <: DSL, FC[_] <: CopK[_]](
    implicit
      toCopK: ToCopK.Aux[F, FC]
    , sub0: SubCop[C, FC]
  ) = new SubDSL[C, F] {
    type Cop[t] = FC[t]
    val sub = sub0
  }

}

trait SubDSL1[F[_], DSL0 <: DSL] {
  type Cop[_] <: CopK[_]

  val sub: SubCop[In1[F, ?], Cop]
}

object SubDSL1 {

  def apply[F[_], DSL0 <: DSL](implicit subdsl: SubDSL1[F, DSL0]) = subdsl

  implicit def subDSL1[F[_], DSL0 <: DSL, FC[_] <: CopK[_]](
    implicit
      toCopK: ToCopK.Aux[DSL0, FC]
    , sub0: SubCop[In1[F, ?], FC]
  ) = new SubDSL1[F, DSL0] {
    type Cop[t] = FC[t]
    val sub = sub0
  }

}

