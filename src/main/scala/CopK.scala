package freek

import cats.~>


/** Higher-Kinded Coproduct (exactly like shapeless Coproduct but higher-kinded)
  *
  * Using shapeless syntax, it represents a M[t] = F[t] :+: G[t] :+: H[t] :+: CNilk[t]
  */ 
sealed trait CopK[A] extends Product with Serializable

sealed trait CNilK[A] extends CopK[A]

// classic model
// sealed trait ConsK[H[_], L[_] <: CopK[_], A] extends CopK[A] {
//   type Cop[t] = ConsK[H, L, t]
// }
// final case class Inlk[H[_], T[_] <: CopK[_], A](head : H[A]) extends ConsK[H, T, A]
// final case class Inrk[H[_], T[_] <: CopK[_], A](tail : T[A]) extends ConsK[H, T, A]

final case class In1[H[_], A](head: H[A]) extends CopK[A]

sealed trait In2[H1[_], H2[_], A] extends CopK[A]
final case class In2l[H1[_], H2[_], A](left: H1[A]) extends In2[H1, H2, A] 
final case class In2r[H1[_], H2[_], A](right: H2[A]) extends In2[H1, H2, A]

sealed trait In3[H1[_], H2[_], H3[_], A] extends CopK[A]
final case class In3l[H1[_], H2[_], H3[_], A](left: H1[A]) extends In3[H1, H2, H3, A]
final case class In3m[H1[_], H2[_], H3[_], A](middle: H2[A]) extends In3[H1, H2, H3, A]
final case class In3r[H1[_], H2[_], H3[_], A](right: H3[A]) extends In3[H1, H2, H3, A]

// Used to lazily delays CopK flattening as long as possible
sealed trait AppendK[L[_] <: CopK[_], R[_] <: CopK[_], A] extends CopK[A]
final case class Aplk[L[_] <: CopK[_], R[_] <: CopK[_], A](left: L[A]) extends AppendK[L, R, A]
final case class Aprk[L[_] <: CopK[_], R[_] <: CopK[_], A](right: R[A]) extends AppendK[L, R, A]

trait ContainsHK[L[_] <: CopK[_], H[_]] extends Serializable {
  def extract[A](la: L[A]): Option[H[A]]
  def build[A](ha: H[A]): L[A]
}


object ContainsHK extends LowerContainsHK {
  
  def apply[L[_] <: CopK[_], H[_]]
    (implicit containsHK: ContainsHK[L, H])/*: Aux[L, H, containsHK.R]*/ = containsHK

  implicit def in1[H[_]]: ContainsHK[In1[H, ?], H] =
    new ContainsHK[In1[H, ?], H] {

      def extract[A](la: In1[H, A]): Option[H[A]] = Some(la.head)

      def build[A](ha: H[A]): In1[H, A] = In1(ha)
    }

  implicit def in2[H1[_], H2[_]]: ContainsHK[In2[H1, H2, ?], H1] =
    new ContainsHK[In2[H1, H2, ?], H1] {

      def extract[A](la: In2[H1, H2, A]): Option[H1[A]] = la match {
        case In2l(l) => Some(l)
        case In2r(r) => None
      }

      def build[A](ha: H1[A]): In2[H1, H2, A] = In2l(ha)
    }

  implicit def in3l[H1[_], H2[_], H3[_]]: ContainsHK[In3[H1, H2, H3, ?], H1] =
    new ContainsHK[In3[H1, H2, H3, ?], H1] {

      def extract[A](la: In3[H1, H2, H3, A]): Option[H1[A]] = la match {
        case In3l(l) => Some(l)
        case In3m(_) => None
        case In3r(_) => None
      }

      def build[A](ha: H1[A]): In3[H1, H2, H3, A] = In3l(ha)
    }

}

trait LowerContainsHK extends LowerContainsHK2 {

  implicit def in2r[H1[_], H2[_]]: ContainsHK[In2[H1, H2, ?], H2] =
    new ContainsHK[In2[H1, H2, ?], H2] {

      def extract[A](la: In2[H1, H2, A]): Option[H2[A]] = la match {
        case In2l(l) => None
        case In2r(r) => Some(r)
      }

      def build[A](ha: H2[A]): In2[H1, H2, A] = In2r(ha)
    }

  implicit def in3m[H1[_], H2[_], H3[_]]: ContainsHK[In3[H1, H2, H3, ?], H2] =
    new ContainsHK[In3[H1, H2, H3, ?], H2] {

      def extract[A](la: In3[H1, H2, H3, A]): Option[H2[A]] = la match {
        case In3l(l) => None
        case In3m(m) => Some(m)
        case In3r(_) => None
      }

      def build[A](ha: H2[A]): In3[H1, H2, H3, A] = In3m(ha)
    }

}


trait LowerContainsHK2 extends LowerContainsHK3 {

  implicit def in3r[H1[_], H2[_], H3[_]]: ContainsHK[In3[H1, H2, H3, ?], H3] =
    new ContainsHK[In3[H1, H2, H3, ?], H3] {

      def extract[A](la: In3[H1, H2, H3, A]): Option[H3[A]] = la match {
        case In3l(_) => None
        case In3m(_) => None
        case In3r(r) => Some(r)
      }

      def build[A](ha: H3[A]): In3[H1, H2, H3, A] = In3r(ha)
    }


  implicit def appendLeft[L1[_] <: CopK[_], L2[_] <: CopK[_], H[_]](
    implicit containsLeft: ContainsHK[L1, H]
  ): ContainsHK[AppendK[L1, L2, ?], H] =
    new ContainsHK[AppendK[L1, L2, ?], H] {

      def extract[A](la: AppendK[L1, L2, A]): Option[H[A]] = la match {
        case Aplk(l) => containsLeft.extract(l)
        case Aprk(_) => None
      }

      def build[A](ha: H[A]): AppendK[L1, L2, A] = Aplk(containsLeft.build(ha))
    }

}

trait LowerContainsHK3 {

  implicit def appendRight[L1[_] <: CopK[_], L2[_] <: CopK[_], H[_]](
    implicit containsRight: ContainsHK[L2, H]
  ): ContainsHK[AppendK[L1, L2, ?], H] =
    new ContainsHK[AppendK[L1, L2, ?], H] {

      def extract[A](la: AppendK[L1, L2, A]): Option[H[A]] = la match {
        case Aplk(_) => None
        case Aprk(r) => containsRight.extract(r)
      }

      def build[A](ha: H[A]): AppendK[L1, L2, A] = Aprk(containsRight.build(ha))
    }

  // implicit def corec[H[_], K[_], L[_] <: CopK[_]](
  //   implicit next: ContainsHK[L, H]
  // ): ContainsHK[ConsK[K, L, ?], H] =
  //   new ContainsHK[ConsK[K, L, ?], H] {

  //     def extract[A](la: ConsK[K, L, A]): Option[H[A]] = la match {
  //       case Inlk(h) => None
  //       case Inrk(r) => next.extract(r)
  //     }

  //     def build[A](ha: H[A]): ConsK[K, L, A] = Inrk(next.build(ha))
  //   }

}



trait SubCop[L[_] <: CopK[_], L2[_] <: CopK[_]] {
  def apply[A](l: L[A]): L2[A]
}

object SubCop extends LowerSubCop {

  def apply[L[_] <: CopK[_], R[_] <: CopK[_]]
    (implicit subCop: SubCop[L, R]): SubCop[L, R] = subCop

  implicit def in1[H[_], L[_] <: CopK[_]](
    implicit contains: ContainsHK[L, H]
  ) = new SubCop[In1[H, ?], L] {
    def apply[A](l: In1[H, A]): L[A] = contains.build(l.head)
  }

  implicit def in2[H1[_], H2[_], L[_] <: CopK[_]](
    implicit contains1: ContainsHK[L, H1], contains2: ContainsHK[L, H2]
  ) = new SubCop[In2[H1, H2, ?], L] {
    def apply[A](l: In2[H1, H2, A]): L[A] = l match {
      case In2l(l) => contains1.build(l)
      case In2r(r) => contains2.build(r)
    }
  }

  implicit def in3[H1[_], H2[_], H3[_], L[_] <: CopK[_]](
    implicit contains1: ContainsHK[L, H1], contains2: ContainsHK[L, H2], contains3: ContainsHK[L, H3]
  ) = new SubCop[In3[H1, H2, H3, ?], L] {
    def apply[A](l: In3[H1, H2, H3, A]): L[A] = l match {
      case In3l(l) => contains1.build(l)
      case In3m(m) => contains2.build(m)
      case In3r(r) => contains3.build(r)
    }
  }

}

trait LowerSubCop extends LowerSubCop2 {

  // implicit def single[H[_], L[_] <: CopK[_]](
  //   implicit contains: ContainsHK[L, H]
  // ) = new SubCop[ConsK[H, CNilK, ?], L] {
  //   def apply[A](l: ConsK[H, CNilK, A]): L[A] = l match {
  //     case Inlk(h) => contains.build(h)
  //     case Inrk(_) => throw new RuntimeException("impossible case")
  //   }
  // }

  implicit def appendkNil[L[_] <: CopK[_], L2[_] <: CopK[_]](
    implicit subLeft: SubCop[L, L2]
  ) = new SubCop[AppendK[L, CNilK, ?], L2] {
    def apply[A](la: AppendK[L, CNilK, A]): L2[A] = la match {
      case Aplk(l) => subLeft(l)
      case Aprk(r) => throw new RuntimeException("impossible case")
    }
  }

}

trait LowerSubCop2 {

  implicit def appendk[L[_] <: CopK[_], R[_] <: CopK[_], L2[_] <: CopK[_]](
    implicit subLeft: SubCop[L, L2], subRight: SubCop[R, L2]
  ) = new SubCop[AppendK[L, R, ?], L2] {
    def apply[A](la: AppendK[L, R, A]): L2[A] = la match {
      case Aplk(l) => subLeft(l)
      case Aprk(r) => subRight(r)
    }
  }

  // implicit def corec[H[_], L[_] <: CopK[_], L2[_] <: CopK[_]](
  //   implicit contains: ContainsHK[L2, H], next: SubCop[L, L2]
  // ) = new SubCop[ConsK[H, L, ?], L2] {
  //   def apply[A](l: ConsK[H, L, A]): L2[A] = l match {
  //     case Inlk(h) => contains.build(h)
  //     case Inrk(r) => next(r)
  //   }
  // }

}

// 



trait PrependHK[H[_], L[_] <: CopK[_]] {
  type Out[_] <: CopK[_]

  def apply[A](ha: L[A]): Out[A]
  def single[A](ha: H[A]): Out[A]

  def nat[R[_], A](out: Out[A], nat1: H ~> R, nat2: L ~> R): R[A]
}

object PrependHK extends PrependHKLower {

  def apply[H[_], L[_] <: CopK[_]]
    (implicit prep: PrependHK[H, L]): Aux[H, L, prep.Out] = prep

  type Aux[H[_], L[_] <: CopK[_], Out0[_] <: CopK[_]] = PrependHK[H, L] { type Out[t] = Out0[t] }

  implicit def in1[H1[_], H2[_]]: Aux[H1, In1[H2, ?], In2[H1, H2, ?]] =
    new PrependHK[H1, In1[H2, ?]] {
      type Out[t] = In2[H1, H2, t]

      def apply[A](c: In1[H2, A]): Out[A] = In2r(c.head)
      def single[A](ha: H1[A]): Out[A] = In2l(ha)

      def nat[R[_], A](out: In2[H1, H2, A], nat1: H1 ~> R, nat2: In1[H2, ?] ~> R): R[A] = out match {
        case In2l(l) => nat1(l)
        case In2r(r) => nat2(In1(r))
      }
    }

  implicit def in2[H1[_], H2[_], H3[_]]: Aux[H1, In2[H2, H3, ?], In3[H1, H2, H3, ?]] =
    new PrependHK[H1, In2[H2, H3, ?]] {
      type Out[t] = In3[H1, H2, H3, t]

      def apply[A](c: In2[H2, H3, A]): Out[A] = c match {
        case In2l(left) => In3m(left)
        case In2r(right) => In3r(right)
      }

      def single[A](ha: H1[A]): Out[A] = In3l(ha)

      def nat[R[_], A](out: In3[H1, H2, H3, A], nat1: H1 ~> R, nat2: In2[H2, H3, ?] ~> R): R[A] = out match {
        case In3l(l) => nat1(l)
        case In3m(m) => nat2(In2l(m))
        case In3r(r) => nat2(In2r(r))
      }
    }

  implicit def in3[H1[_], H2[_], H3[_], H4[_]]: Aux[H1, In3[H2, H3, H4, ?], AppendK[In1[H1, ?], In3[H2, H3, H4, ?], ?]] =
    new PrependHK[H1, In3[H2, H3, H4, ?]] {
      type Out[t] = AppendK[In1[H1, ?], In3[H2, H3, H4, ?], t]

      def apply[A](c: In3[H2, H3, H4, A]): Out[A] = Aprk(c)

      def single[A](ha: H1[A]): Out[A] = Aplk(In1(ha))

      def nat[R[_], A](out: AppendK[In1[H1, ?], In3[H2, H3, H4, ?], A], nat1: H1 ~> R, nat2: In3[H2, H3, H4, ?] ~> R): R[A] = out match {
        case Aplk(In1(l)) => nat1(l)
        case Aprk(m) => nat2(m)
      }
    }

  implicit def append1[H1[_], H2[_], R[_] <: CopK[_], C[_] <: CopK[_]]: Aux[H1, AppendK[In1[H2, ?], R, ?], AppendK[In2[H1, H2, ?], R, ?]] =
    new PrependHK[H1, AppendK[In1[H2, ?], R, ?]] {
      type Out[t] = AppendK[In2[H1, H2, ?], R, t]

      def apply[A](c: AppendK[In1[H2, ?], R, A]): Out[A] = c match {
        case Aplk(In1(l)) => Aplk(In2r(l))
        case Aprk(r) => Aprk(r)
      }

      def single[A](ha: H1[A]): Out[A] = Aplk(In2l(ha))

      def nat[RR[_], A](out: AppendK[In2[H1, H2, ?], R, A], nat1: H1 ~> RR, nat2: AppendK[In1[H2, ?], R, ?] ~> RR): RR[A] = out match {
        case Aplk(In2l(h1)) => nat1(h1)
        case Aplk(In2r(h2)) => nat2(Aplk(In1(h2)))
        case Aprk(r) => nat2(Aprk(r))
      }
    }

  implicit def append2[H1[_], H2[_], H3[_], R[_] <: CopK[_], C[_] <: CopK[_]]: Aux[H1, AppendK[In2[H2, H3, ?], R, ?], AppendK[In3[H1, H2, H3, ?], R, ?]] =
    new PrependHK[H1, AppendK[In2[H2, H3, ?], R, ?]] {
      type Out[t] = AppendK[In3[H1, H2, H3, ?], R, t]

      def apply[A](c: AppendK[In2[H2, H3, ?], R, A]): Out[A] = c match {
        case Aplk(In2l(h2)) => Aplk(In3m(h2))
        case Aplk(In2r(h3)) => Aplk(In3r(h3))
        case Aprk(r) => Aprk(r)
      }

      def single[A](ha: H1[A]): Out[A] = Aplk(In3l(ha))

      def nat[RR[_], A](out: AppendK[In3[H1, H2, H3, ?], R, A], nat1: H1 ~> RR, nat2: AppendK[In2[H2, H3, ?], R, ?] ~> RR): RR[A] = out match {
        case Aplk(In3l(h1)) => nat1(h1)
        case Aplk(In3m(h2)) => nat2(Aplk(In2l(h2)))
        case Aplk(In3r(h3)) => nat2(Aplk(In2r(h3)))
        case Aprk(r) => nat2(Aprk(r))
      }
    }

}

trait PrependHKLower {


  implicit def append[H[_], L[_] <: CopK[_], R[_] <: CopK[_], C[_] <: CopK[_]]: PrependHK.Aux[H, AppendK[L, R, ?], AppendK[In1[H, ?], AppendK[L, R, ?], ?]] =
    new PrependHK[H, AppendK[L, R, ?]] {
      type Out[t] = AppendK[In1[H, ?], AppendK[L, R, ?], t]

      def apply[A](c: AppendK[L, R, A]): Out[A] = Aprk(c)

      def single[A](ha: H[A]): Out[A] = Aplk(In1(ha))

      def nat[RR[_], A](out: AppendK[In1[H, ?], AppendK[L, R, ?], A], nat1: H ~> RR, nat2: AppendK[L, R, ?] ~> RR): RR[A] = out match {
        case Aplk(In1(h)) => nat1(h)
        case Aprk(r) => nat2(r)
      }
    }


}


trait AppendHK[L[_] <: CopK[_], H[_]] {
  type Out[_] <: CopK[_]

  def apply[A](ha: L[A]): Out[A]
  def single[A](ha: H[A]): Out[A]

  def nat[R[_], A](out: Out[A], nat2: L ~> R, nat1: H ~> R): R[A]
}

object AppendHK extends AppendHKLower {

  def apply[L[_] <: CopK[_], H[_]]
    (implicit prep: AppendHK[L, H]): Aux[L, H, prep.Out] = prep

  type Aux[L[_] <: CopK[_], H[_], Out0[_] <: CopK[_]] = AppendHK[L, H] { type Out[t] = Out0[t] }

  implicit def in1[H1[_], H2[_]]: Aux[In1[H1, ?], H2, In2[H1, H2, ?]] =
    new AppendHK[In1[H1, ?], H2] {
      type Out[t] = In2[H1, H2, t]

      def apply[A](c: In1[H1, A]): Out[A] = In2l(c.head)
      def single[A](ha: H2[A]): Out[A] = In2r(ha)

      def nat[R[_], A](out: In2[H1, H2, A], nat1: In1[H1, ?] ~> R, nat2: H2 ~> R): R[A] = out match {
        case In2l(l) => nat1(In1(l))
        case In2r(r) => nat2(r)
      }
    }

  implicit def in2[H1[_], H2[_], H3[_]]: Aux[In2[H1, H2, ?], H3, In3[H1, H2, H3, ?]] =
    new AppendHK[In2[H1, H2, ?], H3] {
      type Out[t] = In3[H1, H2, H3, t]

      def apply[A](c: In2[H1, H2, A]): Out[A] = c match {
        case In2l(left) => In3l(left)
        case In2r(right) => In3m(right)
      }

      def single[A](ha: H3[A]): Out[A] = In3r(ha)

      def nat[R[_], A](out: In3[H1, H2, H3, A], nat1: In2[H1, H2, ?] ~> R, nat2: H3 ~> R): R[A] = out match {
        case In3l(l) => nat1(In2l(l))
        case In3m(m) => nat1(In2r(m))
        case In3r(r) => nat2(r)
      }
    }

  implicit def in3[H1[_], H2[_], H3[_], H4[_]]: Aux[In3[H1, H2, H3, ?], H4, AppendK[In3[H1, H2, H3, ?], In1[H4, ?], ?]] =
    new AppendHK[In3[H1, H2, H3, ?], H4] {
      type Out[t] = AppendK[In3[H1, H2, H3, ?], In1[H4, ?], t]

      def apply[A](c: In3[H1, H2, H3, A]): Out[A] = Aplk(c)

      def single[A](ha: H4[A]): Out[A] = Aprk(In1(ha))

      def nat[R[_], A](out: AppendK[In3[H1, H2, H3, ?], In1[H4, ?], A], nat1: In3[H1, H2, H3, ?] ~> R, nat2: H4 ~> R): R[A] = out match {
        case Aplk(m) => nat1(m)
        case Aprk(In1(l)) => nat2(l)
      }
    }

  implicit def append1[H1[_], H2[_], L[_] <: CopK[_]]: Aux[AppendK[L, In1[H1, ?], ?], H2, AppendK[L, In2[H1, H2, ?], ?]] =
    new AppendHK[AppendK[L, In1[H1, ?], ?], H2] {
      type Out[t] = AppendK[L, In2[H1, H2, ?], t]

      def apply[A](c: AppendK[L, In1[H1, ?], A]): Out[A] = c match {
        case Aplk(l) => Aplk(l)
        case Aprk(In1(r)) => Aprk(In2l(r))
      }

      def single[A](ha: H2[A]): Out[A] = Aprk(In2r(ha))

      def nat[RR[_], A](out: AppendK[L, In2[H1, H2, ?], A], nat1: AppendK[L, In1[H1, ?], ?] ~> RR, nat2: H2 ~> RR): RR[A] = out match {
        case Aplk(r) => nat1(Aplk(r))
        case Aprk(In2l(h1)) => nat1(Aprk(In1(h1)))
        case Aprk(In2r(h2)) => nat2(h2)
      }
    }

  implicit def append2[H1[_], H2[_], H3[_], L[_] <: CopK[_]]: Aux[AppendK[L, In2[H1, H2, ?], ?], H3, AppendK[L, In3[H1, H2, H3, ?], ?]] =
    new AppendHK[AppendK[L, In2[H1, H2, ?], ?], H3] {
      type Out[t] = AppendK[L, In3[H1, H2, H3, ?], t]

      def apply[A](c: AppendK[L, In2[H1, H2, ?], A]): Out[A] = c match {
        case Aplk(r) => Aplk(r)
        case Aprk(In2l(h1)) => Aprk(In3l(h1))
        case Aprk(In2r(h2)) => Aprk(In3m(h2))
      }

      def single[A](ha: H3[A]): Out[A] = Aprk(In3r(ha))

      def nat[RR[_], A](out: AppendK[L, In3[H1, H2, H3, ?], A], nat1: AppendK[L, In2[H1, H2, ?], ?] ~> RR, nat2: H3 ~> RR): RR[A] = out match {
        case Aplk(l) => nat1(Aplk(l))
        case Aprk(In3l(h1)) => nat1(Aprk(In2l(h1)))
        case Aprk(In3m(h2)) => nat1(Aprk(In2r(h2)))
        case Aprk(In3r(h3)) => nat2(h3)
      }
    }

}


trait AppendHKLower {

  implicit def append[H[_], L[_] <: CopK[_], R[_] <: CopK[_]]: AppendHK.Aux[AppendK[L, R, ?], H, AppendK[AppendK[L, R, ?], In1[H, ?], ?]] =
    new AppendHK[AppendK[L, R, ?], H] {
      type Out[t] = AppendK[AppendK[L, R, ?], In1[H, ?], t]

      def apply[A](c: AppendK[L, R, A]): Out[A] = Aplk(c)

      def single[A](ha: H[A]): Out[A] = Aprk(In1(ha))

      def nat[RR[_], A](out: AppendK[AppendK[L, R, ?], In1[H, ?], A], nat1: AppendK[L, R, ?] ~> RR, nat2: H ~> RR): RR[A] = out match {
        case Aplk(l) => nat1(l)
        case Aprk(In1(h)) => nat2(h)
      }
    }


}

trait Replace[C[_] <: CopK[_], F[_], G[_]] {

  type Out[_] <: CopK[_]

  def replace[A](c: C[A])(nat: F ~> G): Out[A] 

}

object Replace extends ReplaceLower {

  type Aux[C[_] <: CopK[_], F[_], G[_], Out0[_] <: CopK[_]] = Replace[C, F, G] { type Out[t] = Out0[t] }

  implicit def in1[F[_], G[_]]: Replace.Aux[In1[F, ?], F, G, In1[G, ?]] = new Replace[In1[F, ?], F, G] {
    type Out[t] = In1[G, t]

    def replace[A](c: In1[F, A])(nat: F ~> G): In1[G, A] = In1(nat(c.head))
  }

  implicit def in2l[F[_], G[_], H[_]]: Replace.Aux[In2[F, G, ?], F, H, In2[H, G, ?]] = new Replace[In2[F, G, ?], F, H] {
    type Out[t] = In2[H, G, t]

    def replace[A](c: In2[F, G, A])(nat: F ~> H): In2[H, G, A] = c match {
      case In2l(l)  => In2l(nat(l))
      case In2r(r)  => In2r(r)
    }
  }

  implicit def in2r[F[_], G[_], H[_]]: Replace.Aux[In2[F, G, ?], G, H, In2[F, H, ?]] = new Replace[In2[F, G, ?], G, H] {
    type Out[t] = In2[F, H, t]

    def replace[A](c: In2[F, G, A])(nat: G ~> H): In2[F, H, A] = c match {
      case In2l(l)  => In2l(l)
      case In2r(r)  => In2r(nat(r))
    }
  }

  implicit def in3l[F[_], G[_], H[_], I[_]]: Replace.Aux[In3[F, G, H, ?], F, I, In3[I, G, H, ?]] = new Replace[In3[F, G, H, ?], F, I] {
    type Out[t] = In3[I, G, H, t]

    def replace[A](c: In3[F, G, H, A])(nat: F ~> I): In3[I, G, H, A] = c match {
      case In3l(l)  => In3l(nat(l))
      case In3m(m)  => In3m(m)
      case In3r(r)  => In3r(r)
    }
  }

  implicit def in3m[F[_], G[_], H[_], I[_]]: Replace.Aux[In3[F, G, H, ?], G, I, In3[F, I, H, ?]] = new Replace[In3[F, G, H, ?], G, I] {
    type Out[t] = In3[F, I, H, t]

    def replace[A](c: In3[F, G, H, A])(nat: G ~> I): In3[F, I, H, A] = c match {
      case In3l(l)  => In3l(l)
      case In3m(m)  => In3m(nat(m))
      case In3r(r)  => In3r(r)
    }
  }

  implicit def in3r[F[_], G[_], H[_], I[_]]: Replace.Aux[In3[F, G, H, ?], H, I, In3[F, G, I, ?]] = new Replace[In3[F, G, H, ?], H, I] {
    type Out[t] = In3[F, G, I, t]

    def replace[A](c: In3[F, G, H, A])(nat: H ~> I): In3[F, G, I, A] = c match {
      case In3l(l)  => In3l(l)
      case In3m(m)  => In3m(m)
      case In3r(r)  => In3r(nat(r))
    }
  }

  implicit def appendl[L[_] <: CopK[_], R[_] <: CopK[_], F[_], G[_], O[_] <: CopK[_]](
    implicit rep: Replace.Aux[L, F, G, O]
  ): Replace.Aux[AppendK[L, R, ?], F, G, AppendK[O, R, ?]] = new Replace[AppendK[L, R, ?], F, G] {
    type Out[t] = AppendK[O, R, t]

    def replace[A](c: AppendK[L, R, A])(nat: F ~> G) = c match {
      case Aplk(l) => Aplk(rep.replace(l)(nat))
      case Aprk(r) => Aprk(r)
    } 
  }
}

trait ReplaceLower {

  implicit def appendr[L[_] <: CopK[_], R[_] <: CopK[_], F[_], G[_], O[_] <: CopK[_]](
    implicit rep: Replace.Aux[R, F, G, O]
  ): Replace.Aux[AppendK[L, R, ?], F, G, AppendK[L, O, ?]] = new Replace[AppendK[L, R, ?], F, G] {
    type Out[t] = AppendK[L, O, t]

    def replace[A](c: AppendK[L, R, A])(nat: F ~> G) = c match {
      case Aplk(l) => Aplk(l)
      case Aprk(r) => Aprk(rep.replace(r)(nat))
    } 
  }
}

trait Flattener[F[_] <: CopK[_], TC[_[_], _]] {
  type Out[_] <: CopK[_]
  def flatten[A](t: TC[F, A]): TC[Out, A]
}

object Flattener extends FlattenerLower{
  import cats.free.Free

  type Aux[F[_] <: CopK[_], TC[_[_], _], Out0[_] <: CopK[_]] = Flattener[F, TC] { type Out[t] = Out0[t] }

  implicit def in1[F[_] <: CopK[_]]: Flattener.Aux[In1[Free[F, ?], ?], Free, F] =
    new Flattener[In1[Free[F, ?], ?], Free] {
      type Out[t] = F[t]

      def flatten[A](tca: Free[In1[Free[F, ?], ?], A]): Free[F, A] = 
        tca.foldMap(new (In1[Free[F, ?], ?] ~> Free[F, ?]) {
          def apply[A](in: In1[Free[F, ?], A]): Free[F, A] = in match {
            case In1(free) => free
          }
        })
    }

  implicit def in2Left[F[_] <: CopK[_], R[_], O[_] <: CopK[_]](
    implicit ap: AppendHK.Aux[F, R, O]
  ): Flattener.Aux[In2[Free[F, ?], R, ?], Free, O] =
    new Flattener[In2[Free[F, ?], R, ?], Free] {
      type Out[t] = O[t]

      def flatten[A](tca: Free[In2[Free[F, ?], R, ?], A]): Free[O, A] = 
        tca.foldMap(new (In2[Free[F, ?], R, ?] ~> Free[O, ?]) {
          def apply[A](in: In2[Free[F, ?], R, A]): Free[O, A] = in match {
            case In2l(free) => free.compile(new (F ~> O) {
              def apply[A](fa: F[A]): O[A] = ap(fa)
            })

            case In2r(r) => Free.liftF(ap.single(r))
          }
        })
    }

  implicit def in3Left[F[_] <: CopK[_], M[_], R[_], O1[_] <: CopK[_], O2[_] <: CopK[_]](
    implicit  ap1: AppendHK.Aux[F, M, O1]
            , ap2: AppendHK.Aux[O1, R, O2]
  ): Flattener.Aux[In3[Free[F, ?], M, R, ?], Free, O2] =
    new Flattener[In3[Free[F, ?], M, R, ?], Free] {
      type Out[t] = O2[t]

      def flatten[A](tca: Free[In3[Free[F, ?], M, R, ?], A]): Free[O2, A] = 
        tca.foldMap(new (In3[Free[F, ?], M, R, ?] ~> Free[O2, ?]) {
          def apply[A](in: In3[Free[F, ?], M, R, A]): Free[O2, A] = in match {
            case In3l(free) => free.compile(new (F ~> O2) {
              def apply[A](fa: F[A]): O2[A] = ap2(ap1(fa))
            })

            case In3m(m) => Free.liftF(ap2(ap1.single(m)))

            case In3r(r) => Free.liftF(ap2.single(r))
          }
        })
    }

}

trait FlattenerLower extends FlattenerLower2 {
  import cats.free.Free

  implicit def in2Right[F[_] <: CopK[_], L[_], O[_] <: CopK[_]](
    implicit pr: PrependHK.Aux[L, F, O]
  ): Flattener.Aux[In2[L, Free[F, ?], ?], Free, O] =
    new Flattener[In2[L, Free[F, ?], ?], Free] {
      type Out[t] = O[t]

      def flatten[A](tca: Free[In2[L, Free[F, ?], ?], A]): Free[O, A] = 
        tca.foldMap(new (In2[L, Free[F, ?], ?] ~> Free[O, ?]) {
          def apply[A](in: In2[L, Free[F, ?], A]): Free[O, A] = in match {
            case In2l(l) => Free.liftF(pr.single(l))

            case In2r(free) => free.compile(new (F ~> O) {
              def apply[A](fa: F[A]): O[A] = pr(fa)
            })
          }
        })
    }

  implicit def in3Middle[F[_] <: CopK[_], L[_], R[_], O1[_] <: CopK[_], O2[_] <: CopK[_]](
    implicit  pr: PrependHK.Aux[L, F, O1]
            , ap: AppendHK.Aux[O1, R, O2]
  ): Flattener.Aux[In3[L, Free[F, ?], R, ?], Free, O2] =
    new Flattener[In3[L, Free[F, ?], R, ?], Free] {
      type Out[t] = O2[t]

      def flatten[A](tca: Free[In3[L, Free[F, ?], R, ?], A]): Free[O2, A] = 
        tca.foldMap(new (In3[L, Free[F, ?], R, ?] ~> Free[O2, ?]) {
          def apply[A](in: In3[L, Free[F, ?], R, A]): Free[O2, A] = in match {
            case In3l(l) => Free.liftF(ap(pr.single(l)))

            case In3m(free) => free.compile(new (F ~> O2) {
              def apply[A](fa: F[A]): O2[A] = ap(pr(fa))
            })

            case In3r(r) => Free.liftF(ap.single(r))
          }
        })
    }


  implicit def apkLeft[F[_] <: CopK[_], L[_] <: CopK[_], R[_] <: CopK[_], O[_] <: CopK[_]](
    implicit flt: Flattener.Aux[L, Free, O]
  ): Flattener.Aux[AppendK[L, R, ?], Free, AppendK[O, R, ?]] = new Flattener[AppendK[L, R, ?], Free] {
    type Out[t] = AppendK[O, R, t]

    def flatten[A](tca: Free[AppendK[L, R, ?], A]): Free[AppendK[O, R, ?], A] = 

      tca.foldMap(new (AppendK[L, R, ?] ~> Free[AppendK[O, R, ?], ?]) {
        def apply[A](in: AppendK[L, R, A]): Free[AppendK[O, R, ?], A] = in match {
          case Aplk(l) =>
            val free = Free.liftF(l)
            flt.flatten(free).compile(new (O ~> AppendK[O, R, ?]) {
              def apply[A](oa: O[A]): AppendK[O, R, A] = Aplk(oa)
            })

          case Aprk(r) => Free.liftF(Aprk(r))
        }
      })
  }

}

trait FlattenerLower2 {
  import cats.free.Free
  
  implicit def in3Right[F[_] <: CopK[_], L[_], M[_], O1[_] <: CopK[_], O2[_] <: CopK[_]](
    implicit  pr1: PrependHK.Aux[M, F, O1]
            , pr2: PrependHK.Aux[L, O1, O2]
  ): Flattener.Aux[In3[L, M, Free[F, ?], ?], Free, O2] =
    new Flattener[In3[L, M, Free[F, ?], ?], Free] {
      type Out[t] = O2[t]

      def flatten[A](tca: Free[In3[L, M, Free[F, ?], ?], A]): Free[O2, A] = 
        tca.foldMap(new (In3[L, M, Free[F, ?], ?] ~> Free[O2, ?]) {
          def apply[A](in: In3[L, M, Free[F, ?], A]): Free[O2, A] = in match {
            case In3l(l) => Free.liftF(pr2.single(l))

            case In3m(m) => Free.liftF(pr2(pr1.single(m)))

            case In3r(free) => free.compile(new (F ~> O2) {
              def apply[A](fa: F[A]): O2[A] = pr2(pr1(fa))
            })

          }
        })
    }


  implicit def ApkRight[F[_] <: CopK[_], L[_] <: CopK[_], R[_] <: CopK[_], O[_] <: CopK[_]](
    implicit flt: Flattener.Aux[R, Free, O]
  ): Flattener.Aux[AppendK[L, R, ?], Free, AppendK[L, O, ?]] = new Flattener[AppendK[L, R, ?], Free] {
    type Out[t] = AppendK[L, O, t]

    def flatten[A](tca: Free[AppendK[L, R, ?], A]): Free[AppendK[L, O, ?], A] = 

      tca.foldMap(new (AppendK[L, R, ?] ~> Free[AppendK[L, O, ?], ?]) {
        def apply[A](in: AppendK[L, R, A]): Free[AppendK[L, O, ?], A] = in match {
          case Aplk(l) =>
            Free.liftF(Aplk(l))

          case Aprk(r) =>
            val free = Free.liftF(r)
            flt.flatten(free).compile(new (O ~> AppendK[L, O, ?]) {
              def apply[A](oa: O[A]): AppendK[L, O, A] = Aprk(oa)
            })

        }
      })
  }
}

// object CopAppend extends CopAppendLower {

//   def apply[L[_] <: CopK[_], R[_] <: CopK[_]](implicit copAppend: CopAppend[L, R]): CopAppend[L, R] = copAppend

//   type Aux[L[_] <: CopK[_], R[_] <: CopK[_], Out0[_] <: CopK[_]] = CopAppend[L, R] {
//     type Out[t] = Out0[t]
//   }

//   implicit def nil[H1[_], H2[_], R2[_] <: CopK[_]]: CopAppend.Aux[AppendK[In1[H1, ?], CNilK, ?], AppendK[In1[H2, ?], R2, ?], AppendK[In1[H1, ?], AppendK[In1[H2, ?], R2, ?], ?]] =
//     new CopAppend[AppendK[In1[H1, ?], CNilK, ?], AppendK[In1[H2, ?], R2, ?]] {
//       type Out[t] = AppendK[In1[H1, ?], AppendK[In1[H2, ?], R2, ?], t]

//       def left[A](l: AppendK[In1[H1, ?], CNilK, A]): Out[A] = l match {
//         case Aplk(in1) => Aplk(in1)
//         case Aprk(_) => throw new RuntimeException("impossible case")
//       }

//       def right[A](l: AppendK[In1[H2, ?], R2, A]): Out[A] = l match {
//         case Aplk(in2) => Aprk(Aplk(in2))
//         case Aprk(r2) => Aprk(Aprk(r2))
//       }

//       def extract[A](o: Out[A]): Xor[AppendK[In1[H1, ?], CNilK, A], AppendK[In1[H2, ?], R2, A]] = o match {
//         case Aplk(In1(h1)) => Xor.left(Aplk(In1(h1)))
//         case Aprk(Aplk(In1(h2))) => Xor.right(Aplk(In1(h2)))
//         case Aprk(Aprk(r2)) => Xor.right(Aprk(r2))
//         case _ => throw new RuntimeException("impossible case")
//       }
//     }
// }

// trait CopAppendLower {
//   implicit def rec[H1[_], R1[_] <: CopK[_], R2[_] <: CopK[_], O[_] <: CopK[_]](
//     implicit next: CopAppend.Aux[R1, R2, O]
//   ): CopAppend.Aux[AppendK[In1[H1, ?], R1, ?], R2, AppendK[In1[H1, ?], O, ?]] =
//     new CopAppend[AppendK[In1[H1, ?], R1, ?], R2] {
//       type Out[t] = AppendK[In1[H1, ?], O, t]

//       def left[A](l: AppendK[In1[H1, ?], R1, A]): Out[A] = l match {
//         case Aplk(in1) => Aplk(in1)
//         case Aprk(r1) => Aprk(next.left(r1))
//       }

//       def right[A](r2: R2[A]): Out[A] = Aprk(next.right(r2))

//        def extract[A](o: Out[A]): Xor[AppendK[In1[H1, ?], R1, A], R2[A]] = o match {
//         case Aplk(In1(h1)) => Xor.left(Aplk(In1(h1)))
//         case Aprk(o) => next.extract(o) match {
//           case Xor.Left(r1) => Xor.left(Aprk(r1))
//           case Xor.Right(r2) => Xor.right(r2)
//         }
//       }
//     }
// }

// trait CopIso[L[_] <: CopK[_], R[_] <: CopK[_]] {
//   def to[A](l: L[A]): R[A]
//   def from[A](r: R[A]): L[A]
// }

// object CopIso extends CopIsoLower {

//   def apply[L[_] <: CopK[_], R[_] <: CopK[_]](implicit copIso: CopIso[L, R]): CopIso[L, R] = copIso

//   implicit def in1[H[_]] = new CopIso[In1[H, ?], AppendK[In1[H, ?], CNilK, ?]] {
//     def to[A](l: In1[H, A]): AppendK[In1[H, ?], CNilK, A] = Aplk(l)
//     def from[A](r: AppendK[In1[H, ?], CNilK, A]): In1[H, A] = r match {
//       case Aplk(l) => l
//       case Aprk(_) => throw new RuntimeException("impossible case")
//     }
//   }

//   implicit def in2[H1[_], H2[_]] = new CopIso[In2[H1, H2, ?], AppendK[In1[H1, ?], AppendK[In1[H2, ?], CNilK, ?], ?]] {
//     def to[A](l: In2[H1, H2, A]): AppendK[In1[H1, ?], AppendK[In1[H2, ?], CNilK, ?], A] = l match {
//       case In2l(l) => Aplk(In1(l))
//       case In2r(r) => Aprk(Aplk(In1(r)))
//     }
//     def from[A](r: AppendK[In1[H1, ?], AppendK[In1[H2, ?], CNilK, ?], A]): In2[H1, H2, A] = r match {
//       case Aplk(In1(l)) => In2l(l)
//       case Aprk(Aplk(In1(r))) => In2r(r)
//       case _ => throw new RuntimeException("impossible case")
//     }
//   }

//   implicit def in3[H1[_], H2[_], H3[_]] = new CopIso[In3[H1, H2, H3, ?], AppendK[In1[H1, ?], AppendK[In1[H2, ?], AppendK[In1[H3, ?], CNilK, ?], ?], ?]] {
//     def to[A](l: In3[H1, H2, H3, A]): AppendK[In1[H1, ?], AppendK[In1[H2, ?], AppendK[In1[H3, ?], CNilK, ?], ?], A] = l match {
//       case In3l(l) => Aplk(In1(l))
//       case In3m(m) => Aprk(Aplk(In1(m)))
//       case In3r(r) => Aprk(Aprk(Aplk(In1(r))))
//     }
//     def from[A](r: AppendK[In1[H1, ?], AppendK[In1[H2, ?], AppendK[In1[H3, ?], CNilK, ?], ?], A]): In3[H1, H2, H3, A] = r match {
//       case Aplk(In1(l)) => In3l(l)
//       case Aprk(Aplk(In1(m))) => In3m(m)
//       case Aprk(Aprk(Aplk(In1(r)))) => In3r(r)
//       case _ => throw new RuntimeException("impossible case")
//     }
//   }

// }

// trait CopIsoLower {
//   implicit def rec[L[_] <: CopK[_], R[_] <: CopK[_], OL[_] <: CopK[_], OR[_] <: CopK[_], O[_] <: CopK[_]](
//     implicit
//       leftIso: CopIso[L, OL]
//     , rightIso: CopIso[R, OR]
//     , ap: CopAppend.Aux[OL, OR, O]
//   ) = new CopIso[AppendK[L, R, ?], O] {

//     def to[A](l: AppendK[L, R, A]): O[A] = l match {
//       case Aplk(l) => ap.left(leftIso.to(l))
//       case Aprk(r) => ap.right(rightIso.to(r))
//     }
//     def from[A](o: O[A]): AppendK[L, R, A] = ap.extract(o) match {
//       case Xor.Left(l) => Aplk(leftIso.from(l))
//       case Xor.Right(r) => Aprk(rightIso.from(r))
//     }
//   }
// }

// trait CopAppend[L[_] <: CopK[_], R[_] <: CopK[_]] {
//   type Out[_] <: CopK[_]

//   def left[A](l: L[A]): Out[A]
//   def right[A](l: R[A]): Out[A]
//   def extract[A](o: Out[A]): Xor[L[A], R[A]]
// }

// trait MergeOneRightHK[L[_] <: CopK[_], H[_]] {
//   type Out[_] <: CopK[_]

//   def apply[A](ha: L[A]): Out[A]
//   def single[A](ha: H[A]): Out[A]
// }

// object MergeOneRightHK extends LowerMergeOneRightHK {

//   def apply[L[_] <: CopK[_], H[_]]
//     (implicit mergeOneRightHK: MergeOneRightHK[L, H]): Aux[L, H, mergeOneRightHK.Out] = mergeOneRightHK

//   type Aux[L[_] <: CopK[_], H[_], Out0[_] <: CopK[_]] = MergeOneRightHK[L, H] { type Out[t] = Out0[t] }

//   implicit def singleton[H[_], G[_]]: Aux[ConsK[H, CNilK, ?], G, ConsK[H, ConsK[G, CNilK, ?], ?]] =
//     new MergeOneRightHK[ConsK[H, CNilK, ?], G] {
//       type Out[t] = ConsK[H, ConsK[G, CNilK, ?], t]

//       def apply[A](c: ConsK[H, CNilK, A]): Out[A] = c match {
//         case Inlk(h) => Inlk(h)
//         case Inrk(t) => Inrk(Inrk(t))
//       }

//       def single[A](ga: G[A]): Out[A] = Inrk(Inlk(ga))
//     }

// }

// trait LowerMergeOneRightHK extends LowerMergeOneRightHK2 {

//   implicit def contains[H[_], T[_] <: CopK[_]]
//     (implicit
//       contains: ContainsHK[T, H]
//     ): MergeOneRightHK.Aux[T, H, T] =
//       new MergeOneRightHK[T, H] {
//         type Out[t] = T[t]

//         def apply[A](c: T[A]): Out[A] = c

//         def single[A](ha: H[A]): Out[A] = contains.build(ha)
//       }

// }

// trait LowerMergeOneRightHK2 {
//   implicit def corec[H[_], K[_], T[_] <: CopK[_], T2[_] <: CopK[_]]
//     (implicit next: MergeOneRightHK.Aux[T, H, T2]): MergeOneRightHK.Aux[ConsK[K, T, ?], H, ConsK[K, T2, ?]] =
//       new MergeOneRightHK[ConsK[K, T, ?], H] {
//         type Out[t] = ConsK[K, T2, t]

//         def apply[A](c: ConsK[K, T, A]): Out[A] = c match {
//           case Inlk(h) => Inlk(h)
//           case Inrk(t) => Inrk(next(t))
//         }

//         def single[A](ha: H[A]): Out[A] = Inrk(next.single(ha))
//       }
// }

// trait MergeCopHK[L[_] <: CopK[_], R[_] <: CopK[_]] {
//   type Out[_] <: CopK[_]

//   def fromLeft[A](la: L[A]): Out[A]

//   def fromRight[A](ra: R[A]): Out[A]
// }


// object MergeCopHK extends LowerMergeCopHK {
  
//   def apply[L[_] <: CopK[_], R[_] <: CopK[_]]
//     (implicit mergeCopHK: MergeCopHK[L, R]): Aux[L, R, mergeCopHK.Out] = mergeCopHK

//   type Aux[L[_] <: CopK[_], R[_] <: CopK[_], Out0[_] <: CopK[_]] = MergeCopHK[L, R] { type Out[t] = Out0[t] }

//   implicit def one[L[_] <: CopK[_], H[_], LH[_] <: CopK[_]](
//     implicit mergeOne: MergeOneRightHK.Aux[L, H, LH]
//   ): Aux[L, ConsK[H, CNilK, ?], LH] = 
//     new MergeCopHK[L, ConsK[H, CNilK, ?]] {
//       type Out[t] = LH[t]

//       def fromLeft[A](la: L[A]): Out[A] = mergeOne(la)

//       def fromRight[A](ra: ConsK[H, CNilK, A]): Out[A] = ra match {
//         case Inlk(ha) => mergeOne.single(ha)
//         case Inrk(_) => throw new RuntimeException("impossible case")
//       }
//     }
// }

// trait LowerMergeCopHK {
//   implicit def corec[L[_] <: CopK[_], H[_], LH[_] <: CopK[_], T[_] <: CopK[_], T2[_] <: CopK[_]](
//     implicit mergeOne: MergeOneRightHK.Aux[L, H, LH], next: MergeCopHK.Aux[LH, T, T2]
//   ): MergeCopHK.Aux[L, ConsK[H, T, ?], T2] =
//     new MergeCopHK[L, ConsK[H, T, ?]] {
//       type Out[t] = T2[t]

//       def fromLeft[A](la: L[A]): Out[A] = next.fromLeft(mergeOne(la))
//       def fromRight[A](ra: ConsK[H, T, A]): Out[A] = ra match {
//         case Inlk(ha) => next.fromLeft(mergeOne.single(ha))
//         case Inrk(ta) => next.fromRight(ta)
//       }
//     }
// }



/*trait ContainsHKLub[L[_] <: CopK[_], H[_]] extends Serializable {
  type R[_] <: CopK[_]

  type Lub[_]

  def extract[A](la: L[A]): Option[Lub[A]]
  def build[A](ha: H[A]): L[A]
}


object ContainsHKLub extends LowerContainsHKLub {

  type Aux[L[_] <: CopK[_], H[_], R0[_] <: CopK[_], Lub0[_]] =
    ContainsHKLub[L, H] { type R[t] = R0[t]; type Lub[t] = Lub0[t] }
  
  def apply[L[_] <: CopK[_], H[_]]
    (implicit containsHK: ContainsHKLub[L, H]): Aux[L, H, containsHK.R, containsHK.Lub] = containsHK

  implicit def head[H[_], K[_], L[_] <: CopK[_]](
    implicit ev: H[_] <:< K[_]
  ): ContainsHKLub.Aux[ConsK[K, L, ?], H, L, K] =
    new ContainsHKLub[ConsK[K, L, ?], H] {
      type R[t] = L[t]
      type Lub[t] = K[t]

      def extract[A](la: ConsK[K, L, A]): Option[K[A]] = la match {
        case Inlk(h) => Some(h)
        case Inrk(_) => None
      }

      def build[A](ha: H[A]): ConsK[K, L, A] = Inlk(ha.asInstanceOf[K[A]])
    }

}


trait LowerContainsHKLub {

  implicit def corec[H[_], K[_], L[_] <: CopK[_], RT[_] <: CopK[_], RTLub[_]](
    implicit next: ContainsHKLub.Aux[L, H, RT, RTLub]
  ): ContainsHKLub.Aux[ConsK[K, L, ?], H, ConsK[K, RT, ?], RTLub] =
    new ContainsHKLub[ConsK[K, L, ?], H] {
      type R[t] = ConsK[K, RT, t]
      type Lub[t] = RTLub[t]

      def extract[A](la: ConsK[K, L, A]): Option[RTLub[A]] = la match {
        case Inlk(h) => None
        case Inrk(r) => next.extract(r)
      }

      def build[A](ha: H[A]): ConsK[K, L, A] = Inrk(next.build(ha))
    }
}*/



