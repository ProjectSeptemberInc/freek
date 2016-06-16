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
  * type C[t] = (G :@: H :@: FXNil)#Cop[t] (gives [t => G[t] :+: H[t] :+: CNilK[t])
  *
  * type C2[t] = (F :@@: C)#Cop[t] (gives [t => F[t] :+: G[t] :+: H[t] :+: CNilK[t])
  * ```
  */
sealed trait FX {
  type Cop[_] <: CoproductK[_]
}

sealed trait :|:[H[_], T <: FX] extends FX {
  type Cop[t] = ConsK[H, T#Cop, t]
}


sealed trait FXNil extends FX {
  type Cop[t] = CNilK[t]
}


