package freek

import cats.~>

/** Higher-Kinded Coproduct (exactly like shapeless Coproduct but higher-kinded)
  *
  * Using shapeless syntax, it represents a M[t] = F[t] :+: G[t] :+: H[t] :+: CNilk[t]
  */ 
sealed trait CoproductK[A] extends Product with Serializable {
  type Cop[t] <: CoproductK[t]
}

// sealed trait CNilK[A] extends CoproductK[A]
sealed trait CNilK[A] extends CoproductK[A] {
  type Cop[t] = CNilK[t]
}

sealed trait ConsK[H[_], L[_] <: CoproductK[_], A] extends CoproductK[A] {
  type Cop[t] = ConsK[H, L, t]
}
final case class Inlk[H[_], T[_] <: CoproductK[_], A](head : H[A]) extends ConsK[H, T, A]
final case class Inrk[H[_], T[_] <: CoproductK[_], A](tail : T[A]) extends ConsK[H, T, A]

final case class In1[H[_], A](head: H[A]) extends CoproductK[A] with ConsK[H, CNilK, A] {
  override type Cop[t] = ConsK[H, CNilK, t]
}

sealed trait In2[H1[_], H2[_], A] extends CoproductK[A] with ConsK[H1, ConsK[H2, CNilK, ?], A] {
  override type Cop[t] = ConsK[H1, ConsK[H2, CNilK, ?], t]
}
final case class In2l[H1[_], H2[_], A](left: H1[A]) extends In2[H1, H2, A] 

final case class In2r[H1[_], H2[_], A](right: H2[A]) extends In2[H1, H2, A]

sealed trait In3[H1[_], H2[_], H3[_], A] extends CoproductK[A] with ConsK[H1, ConsK[H2, ConsK[H3, CNilK, ?], ?], A] {
  override type Cop[t] = ConsK[H1, ConsK[H2, ConsK[H3, CNilK, ?], ?], t]
}

final case class In3l[H1[_], H2[_], H3[_], A](left: H1[A]) extends In3[H1, H2, H3, A]
final case class In3m[H1[_], H2[_], H3[_], A](middle: H2[A]) extends In3[H1, H2, H3, A]
final case class In3r[H1[_], H2[_], H3[_], A](right: H3[A]) extends In3[H1, H2, H3, A]

// Used to lazily delays CoproductK flattening as long as possible
sealed trait AppendK[L[_] <: CoproductK[_], R[_] <: CoproductK[_], A] extends CoproductK[A]
final case class Aplk[L[_] <: CoproductK[_], R[_] <: CoproductK[_], A](left: L[A]) extends AppendK[L, R, A]
final case class Aprk[L[_] <: CoproductK[_], R[_] <: CoproductK[_], A](right: R[A]) extends AppendK[L, R, A]


trait ContainsHK[L[_] <: CoproductK[_], H[_]] extends Serializable {
  def extract[A](la: L[A]): Option[H[A]]
  def build[A](ha: H[A]): L[A]
}


object ContainsHK extends LowerContainsHK {
  
  def apply[L[_] <: CoproductK[_], H[_]]
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

  implicit def in2r[H1[_], H2[_]]: ContainsHK[In2[H1, H2, ?], H2] =
    new ContainsHK[In2[H1, H2, ?], H2] {

      def extract[A](la: In2[H1, H2, A]): Option[H2[A]] = la match {
        case In2l(l) => None
        case In2r(r) => Some(r)
      }

      def build[A](ha: H2[A]): In2[H1, H2, A] = In2r(ha)
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

  implicit def in3m[H1[_], H2[_], H3[_]]: ContainsHK[In3[H1, H2, H3, ?], H2] =
    new ContainsHK[In3[H1, H2, H3, ?], H2] {

      def extract[A](la: In3[H1, H2, H3, A]): Option[H2[A]] = la match {
        case In3l(l) => None
        case In3m(m) => Some(m)
        case In3r(_) => None
      }

      def build[A](ha: H2[A]): In3[H1, H2, H3, A] = In3m(ha)
    }

  implicit def in3r[H1[_], H2[_], H3[_]]: ContainsHK[In3[H1, H2, H3, ?], H3] =
    new ContainsHK[In3[H1, H2, H3, ?], H3] {

      def extract[A](la: In3[H1, H2, H3, A]): Option[H3[A]] = la match {
        case In3l(_) => None
        case In3m(_) => None
        case In3r(r) => Some(r)
      }

      def build[A](ha: H3[A]): In3[H1, H2, H3, A] = In3r(ha)
    }

  implicit def head[H[_], L[_] <: CoproductK[_]]: ContainsHK[ConsK[H, L, ?], H] =
    new ContainsHK[ConsK[H, L, ?], H] {

      def extract[A](la: ConsK[H, L, A]): Option[H[A]] = la match {
        case Inlk(h) => Some(h)
        case Inrk(_) => None
      }

      def build[A](ha: H[A]): ConsK[H, L, A] = Inlk(ha)
    }

  implicit def appendLeft[L1[_] <: CoproductK[_], L2[_] <: CoproductK[_], H[_]](
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


trait LowerContainsHK {

  implicit def appendRight[L1[_] <: CoproductK[_], L2[_] <: CoproductK[_], H[_]](
    implicit containsRight: ContainsHK[L2, H]
  ): ContainsHK[AppendK[L1, L2, ?], H] =
    new ContainsHK[AppendK[L1, L2, ?], H] {

      def extract[A](la: AppendK[L1, L2, A]): Option[H[A]] = la match {
        case Aplk(_) => None
        case Aprk(r) => containsRight.extract(r)
      }

      def build[A](ha: H[A]): AppendK[L1, L2, A] = Aprk(containsRight.build(ha))
    }

  implicit def corec[H[_], K[_], L[_] <: CoproductK[_]](
    implicit next: ContainsHK[L, H]
  ): ContainsHK[ConsK[K, L, ?], H] =
    new ContainsHK[ConsK[K, L, ?], H] {

      def extract[A](la: ConsK[K, L, A]): Option[H[A]] = la match {
        case Inlk(h) => None
        case Inrk(r) => next.extract(r)
      }

      def build[A](ha: H[A]): ConsK[K, L, A] = Inrk(next.build(ha))
    }

}



trait SubCop[L[_] <: CoproductK[_], L2[_] <: CoproductK[_]] {
  def apply[A](l: L[A]): L2[A]
}

object SubCop extends LowerSubCop {

  def apply[L[_] <: CoproductK[_], R[_] <: CoproductK[_]]
    (implicit subCop: SubCop[L, R]): SubCop[L, R] = subCop

  implicit def single[H[_], L[_] <: CoproductK[_]](
    implicit contains: ContainsHK[L, H]
  ) = new SubCop[ConsK[H, CNilK, ?], L] {
    def apply[A](l: ConsK[H, CNilK, A]): L[A] = l match {
      case Inlk(h) => contains.build(h)
      case Inrk(_) => throw new RuntimeException("impossible case")
    }
  }

  implicit def in1[H[_], L[_] <: CoproductK[_]](
    implicit contains: ContainsHK[L, H]
  ) = new SubCop[In1[H, ?], L] {
    def apply[A](l: In1[H, A]): L[A] = contains.build(l.head)
  }

  implicit def in2[H1[_], H2[_], L[_] <: CoproductK[_]](
    implicit contains1: ContainsHK[L, H1], contains2: ContainsHK[L, H2]
  ) = new SubCop[In2[H1, H2, ?], L] {
    def apply[A](l: In2[H1, H2, A]): L[A] = l match {
      case In2l(l) => contains1.build(l)
      case In2r(r) => contains2.build(r)
    }
  }

  implicit def in3[H1[_], H2[_], H3[_], L[_] <: CoproductK[_]](
    implicit contains1: ContainsHK[L, H1], contains2: ContainsHK[L, H2], contains3: ContainsHK[L, H3]
  ) = new SubCop[In3[H1, H2, H3, ?], L] {
    def apply[A](l: In3[H1, H2, H3, A]): L[A] = l match {
      case In3l(l) => contains1.build(l)
      case In3m(m) => contains2.build(m)
      case In3r(r) => contains3.build(r)
    }
  }

}

trait LowerSubCop {

  implicit def appendk[H[_], L[_] <: CoproductK[_], R[_] <: CoproductK[_], L2[_] <: CoproductK[_]](
    implicit subLeft: SubCop[L, L2], subRight: SubCop[R, L2]
  ) = new SubCop[AppendK[L, R, ?], L2] {
    def apply[A](la: AppendK[L, R, A]): L2[A] = la match {
      case Aplk(l) => subLeft(l)
      case Aprk(r) => subRight(r)
    }
  }

  implicit def corec[H[_], L[_] <: CoproductK[_], L2[_] <: CoproductK[_]](
    implicit contains: ContainsHK[L2, H], next: SubCop[L, L2]
  ) = new SubCop[ConsK[H, L, ?], L2] {
    def apply[A](l: ConsK[H, L, A]): L2[A] = l match {
      case Inlk(h) => contains.build(h)
      case Inrk(r) => next(r)
    }
  }

}

trait PrependHK[H[_], L[_] <: CoproductK[_]] {
  type Out[_] <: CoproductK[_]

  def apply[A](ha: L[A]): Out[A]
  def single[A](ha: H[A]): Out[A]

  def nat[R[_], A](out: Out[A], nat1: H ~> R, nat2: L ~> R): R[A]
}

object PrependHK {

  def apply[H[_], L[_] <: CoproductK[_]]
    (implicit prep: PrependHK[H, L]): Aux[H, L, prep.Out] = prep

  type Aux[H[_], L[_] <: CoproductK[_], Out0[_] <: CoproductK[_]] = PrependHK[H, L] { type Out[t] = Out0[t] }

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

  implicit def append[H[_], L[_] <: CoproductK[_], R[_] <: CoproductK[_], C[_] <: CoproductK[_]](
    implicit prep: PrependHK.Aux[H, L, C]
  ): Aux[H, AppendK[L, R, ?], AppendK[C, R, ?]] =
    new PrependHK[H, AppendK[L, R, ?]] {
      type Out[t] = AppendK[C, R, t]

      def apply[A](c: AppendK[L, R, A]): Out[A] = c match {
        case Aplk(l) => Aplk(prep(l))
        case Aprk(r) => Aprk(r)
      }

      def single[A](ha: H[A]): Out[A] = Aplk(prep.single(ha))

      def nat[RR[_], A](out: AppendK[C, R, A], nat1: H ~> RR, nat2: AppendK[L, R, ?] ~> RR): RR[A] = out match {
        case Aplk(l) => prep.nat(l, nat1, new (L ~> RR) {
          def apply[A](fa: L[A]): RR[A] = nat2(Aplk(fa))
        })
        case Aprk(m) => nat2(Aprk(m))
      }
    }

}

trait MergeOneRightHK[L[_] <: CoproductK[_], H[_]] {
  type Out[_] <: CoproductK[_]

  def apply[A](ha: L[A]): Out[A]
  def single[A](ha: H[A]): Out[A]
}

object MergeOneRightHK extends LowerMergeOneRightHK {

  def apply[L[_] <: CoproductK[_], H[_]]
    (implicit mergeOneRightHK: MergeOneRightHK[L, H]): Aux[L, H, mergeOneRightHK.Out] = mergeOneRightHK

  type Aux[L[_] <: CoproductK[_], H[_], Out0[_] <: CoproductK[_]] = MergeOneRightHK[L, H] { type Out[t] = Out0[t] }

  implicit def singleton[H[_], G[_]]: Aux[ConsK[H, CNilK, ?], G, ConsK[H, ConsK[G, CNilK, ?], ?]] =
    new MergeOneRightHK[ConsK[H, CNilK, ?], G] {
      type Out[t] = ConsK[H, ConsK[G, CNilK, ?], t]

      def apply[A](c: ConsK[H, CNilK, A]): Out[A] = c match {
        case Inlk(h) => Inlk(h)
        case Inrk(t) => Inrk(Inrk(t))
      }

      def single[A](ga: G[A]): Out[A] = Inrk(Inlk(ga))
    }

}

trait LowerMergeOneRightHK extends LowerMergeOneRightHK2 {

  implicit def contains[H[_], T[_] <: CoproductK[_]]
    (implicit
      contains: ContainsHK[T, H]
    ): MergeOneRightHK.Aux[T, H, T] =
      new MergeOneRightHK[T, H] {
        type Out[t] = T[t]

        def apply[A](c: T[A]): Out[A] = c

        def single[A](ha: H[A]): Out[A] = contains.build(ha)
      }

}

trait LowerMergeOneRightHK2 {
  implicit def corec[H[_], K[_], T[_] <: CoproductK[_], T2[_] <: CoproductK[_]]
    (implicit next: MergeOneRightHK.Aux[T, H, T2]): MergeOneRightHK.Aux[ConsK[K, T, ?], H, ConsK[K, T2, ?]] =
      new MergeOneRightHK[ConsK[K, T, ?], H] {
        type Out[t] = ConsK[K, T2, t]

        def apply[A](c: ConsK[K, T, A]): Out[A] = c match {
          case Inlk(h) => Inlk(h)
          case Inrk(t) => Inrk(next(t))
        }

        def single[A](ha: H[A]): Out[A] = Inrk(next.single(ha))
      }
}

trait MergeCopHK[L[_] <: CoproductK[_], R[_] <: CoproductK[_]] {
  type Out[_] <: CoproductK[_]

  def fromLeft[A](la: L[A]): Out[A]

  def fromRight[A](ra: R[A]): Out[A]
}


object MergeCopHK extends LowerMergeCopHK {
  
  def apply[L[_] <: CoproductK[_], R[_] <: CoproductK[_]]
    (implicit mergeCopHK: MergeCopHK[L, R]): Aux[L, R, mergeCopHK.Out] = mergeCopHK

  type Aux[L[_] <: CoproductK[_], R[_] <: CoproductK[_], Out0[_] <: CoproductK[_]] = MergeCopHK[L, R] { type Out[t] = Out0[t] }

  implicit def one[L[_] <: CoproductK[_], H[_], LH[_] <: CoproductK[_]](
    implicit mergeOne: MergeOneRightHK.Aux[L, H, LH]
  ): Aux[L, ConsK[H, CNilK, ?], LH] = 
    new MergeCopHK[L, ConsK[H, CNilK, ?]] {
      type Out[t] = LH[t]

      def fromLeft[A](la: L[A]): Out[A] = mergeOne(la)

      def fromRight[A](ra: ConsK[H, CNilK, A]): Out[A] = ra match {
        case Inlk(ha) => mergeOne.single(ha)
        case Inrk(_) => throw new RuntimeException("impossible case")
      }
    }
}

trait LowerMergeCopHK {
  implicit def corec[L[_] <: CoproductK[_], H[_], LH[_] <: CoproductK[_], T[_] <: CoproductK[_], T2[_] <: CoproductK[_]](
    implicit mergeOne: MergeOneRightHK.Aux[L, H, LH], next: MergeCopHK.Aux[LH, T, T2]
  ): MergeCopHK.Aux[L, ConsK[H, T, ?], T2] =
    new MergeCopHK[L, ConsK[H, T, ?]] {
      type Out[t] = T2[t]

      def fromLeft[A](la: L[A]): Out[A] = next.fromLeft(mergeOne(la))
      def fromRight[A](ra: ConsK[H, T, A]): Out[A] = ra match {
        case Inlk(ha) => next.fromLeft(mergeOne.single(ha))
        case Inrk(ta) => next.fromRight(ta)
      }
    }
}



/*trait ContainsHKLub[L[_] <: CoproductK[_], H[_]] extends Serializable {
  type R[_] <: CoproductK[_]

  type Lub[_]

  def extract[A](la: L[A]): Option[Lub[A]]
  def build[A](ha: H[A]): L[A]
}


object ContainsHKLub extends LowerContainsHKLub {

  type Aux[L[_] <: CoproductK[_], H[_], R0[_] <: CoproductK[_], Lub0[_]] =
    ContainsHKLub[L, H] { type R[t] = R0[t]; type Lub[t] = Lub0[t] }
  
  def apply[L[_] <: CoproductK[_], H[_]]
    (implicit containsHK: ContainsHKLub[L, H]): Aux[L, H, containsHK.R, containsHK.Lub] = containsHK

  implicit def head[H[_], K[_], L[_] <: CoproductK[_]](
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

  implicit def corec[H[_], K[_], L[_] <: CoproductK[_], RT[_] <: CoproductK[_], RTLub[_]](
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



