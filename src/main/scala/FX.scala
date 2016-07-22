package freek

/** a type helper to build Coproduct of effects F[_] with a clean syntax
  *
  * FXNil is equivalent to higher-kinded CNil
  * to build the equivalent of [t => F[t] :+: G[t] :+: CNilk[t]], use following syntax
  *
  * ```
  * type F[_]
  * type G[_]
  * type H[_]
  *
  * type C[t] = (G :|: H :|: FXNil)#Cop[t] (gives [t => G[t] :+: H[t] :+: CNilK[t])
  *
  * type C2[t] = (F :||: C)#Cop[t] (gives [t => F[t] :+: G[t] :+: H[t] :+: CNilK[t])
  * ```
  */
sealed trait FX

class :|:[H[_], T <: FX](implicit val toCop:ToCopK[H :|: T]) extends FX

trait :||:[T1 <: FX, T2 <: FX] extends FX

trait FXNil extends FX


class Program[FX0 <: FX, C[_] <: CoproductK[_]] {
  type Cop[t] = C[t]
}

object Program {
  def apply[FX0 <: FX](implicit toCop:ToCopK[FX0]) = new Program[FX0, toCop.Cop] {}
}

trait ToCopK[F <: FX] {
  type Cop[_] <: CoproductK[_]
}

object ToCopK extends LowerToCopK {

  def apply[F <: FX](implicit toCopK: ToCopK[F]) = toCopK

  type Aux[F <: FX, C[_] <: CoproductK[_]] = ToCopK[F] {
    type Cop[t] = C[t]
  }

  implicit val fxnil: ToCopK.Aux[FXNil, CNilK] = new ToCopK[FXNil] {
    type Cop[t] = CNilK[t]
  }

  implicit def one[H[_]]: ToCopK.Aux[:|:[H, FXNil], In1[H, ?]] =
    new ToCopK[:|:[H, FXNil]] {
      type Cop[t] = In1[H, t]
    }

  implicit def two[H[_], H2[_]]: ToCopK.Aux[:|:[H, :|:[H2, FXNil]], In2[H, H2, ?]] =
    new ToCopK[:|:[H, :|:[H2, FXNil]]] {
      type Cop[t] = In2[H, H2, t]
    }

  implicit def three[H[_], H2[_], H3[_]]: ToCopK.Aux[:|:[H, :|:[H2, :|:[H3, FXNil]]], In3[H, H2, H3, ?]] =
    new ToCopK[:|:[H, :|:[H2, :|:[H3, FXNil]]]] {
      type Cop[t] = In3[H, H2, H3, t]
    }

}

trait LowerToCopK {

  implicit def rec[H[_], T <: FX, C[_] <: CoproductK[_], O[_] <: CoproductK[_]](
    implicit
      next: ToCopK.Aux[T, C]
    , prep: PrependHK.Aux[H, C, O]
  ): ToCopK.Aux[:|:[H, T], O] =
    new ToCopK[:|:[H, T]] {
      type Cop[t] = O[t]
    }

  implicit def merge[T1 <: FX, T2 <: FX, C1[_] <: CoproductK[_], C2[_] <: CoproductK[_]](
    implicit
      toCopK1: ToCopK.Aux[T1, C1]
    , toCopK2: ToCopK.Aux[T2, C2]
  ): ToCopK.Aux[:||:[T1, T2], AppendK[C1, C2, ?]] =
    new ToCopK[:||:[T1, T2]] {
      type Cop[t] = AppendK[C1, C2, t]
    }

}

trait SubFX[C[_] <: CoproductK[_], F <: FX] {
  type Cop[_] <: CoproductK[_]

  val sub: SubCop[C, Cop]
}

object SubFX {

  def apply[C[_] <: CoproductK[_], F <: FX](implicit subfxfx: SubFX[C, F]) = subfxfx

  implicit def subfx[C[_] <: CoproductK[_], F <: FX, FC[_] <: CoproductK[_]](
    implicit
      toCopK: ToCopK.Aux[F, FC]
    , sub0: SubCop[C, FC]
  ) = new SubFX[C, F] {
    type Cop[t] = FC[t]
    val sub = sub0
  }

}

