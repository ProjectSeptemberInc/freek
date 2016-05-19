package freek

/** Higher-Kinded Coproduct (exactly like shapeless Coproduct but higher-kinded)
  *
  * Using shapeless syntax, it represents a M[t] = F[t] :+: G[t] :+: H[t] :+: CNilk[t]
  */ 
sealed trait CoproductK[A] extends Product with Serializable

// sealed trait CNilK[A] extends CoproductK[A]
sealed trait CNilK[A] extends CoproductK[A]

sealed trait ConsK[H[_], L[_] <: CoproductK[_], A] extends CoproductK[A]
final case class Inlk[H[_], T[_] <: CoproductK[_], A](head : H[A]) extends ConsK[H, T, A]
final case class Inrk[H[_], T[_] <: CoproductK[_], A](tail : T[A]) extends ConsK[H, T, A]


trait ContainsHK[L[_] <: CoproductK[_], H[_]] extends Serializable {
  type R[_] <: CoproductK[_]

  def extract[A](la: L[A]): Option[H[A]]
  def build[A](ha: H[A]): L[A]
}


object ContainsHK extends LowerContainsHK {

  type Aux[L[_] <: CoproductK[_], H[_], R0[_] <: CoproductK[_]] = ContainsHK[L, H] { type R[t] = R0[t] }
  
  def apply[L[_] <: CoproductK[_], H[_]]
    (implicit containsHK: ContainsHK[L, H]): Aux[L, H, containsHK.R] = containsHK

  implicit def singleton[H[_], H1[t] <: H[t]]: Aux[ConsK[H, CNilK, ?], H1, CNilK] =
    new ContainsHK[ConsK[H, CNilK, ?], H1] {
      type R[t] = CNilK[t]

      def extract[A](la: ConsK[H, CNilK, A]): Option[H1[A]] = la match {
        case Inlk(h) => Some(h.asInstanceOf[H1[A]])
        case Inrk(_) => None
      }

      def build[A](ha: H1[A]): ConsK[H, CNilK, A] = Inlk(ha.asInstanceOf[H[A]])
    }

}


trait LowerContainsHK {

  implicit def head[H[_], L[_] <: CoproductK[_]]: ContainsHK.Aux[ConsK[H, L, ?], H, L] =
    new ContainsHK[ConsK[H, L, ?], H] {
      type R[t] = L[t]

      def extract[A](la: ConsK[H, L, A]): Option[H[A]] = la match {
        case Inlk(h) => Some(h)
        case Inrk(_) => None
      }

      def build[A](ha: H[A]): ConsK[H, L, A] = Inlk(ha)
    }

  implicit def corec[H[_], K[_], L[_] <: CoproductK[_], RT[_] <: CoproductK[_]](
    implicit next: ContainsHK.Aux[L, H, RT]
  ): ContainsHK.Aux[ConsK[K, L, ?], H, ConsK[K, RT, ?]] =
    new ContainsHK[ConsK[K, L, ?], H] {
      type R[t] = ConsK[K, RT, t]

      def extract[A](la: ConsK[K, L, A]): Option[H[A]] = la match {
        case Inlk(h) => None
        case Inrk(r) => next.extract(r)
      }

      def build[A](ha: H[A]): ConsK[K, L, A] = Inrk(next.build(ha))
    }
}


trait MergeOneRightHK[L[_] <: CoproductK[_], H[_]] extends Serializable {
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

trait MergeCopHK[L[_] <: CoproductK[_], R[_] <: CoproductK[_]] extends Serializable {
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



trait SubCop[L[_] <: CoproductK[_], L2[_] <: CoproductK[_]] extends Serializable {
  def apply[A](l: L[A]): L2[A]
}

object SubCop {

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

  implicit def corec[H[_], L[_] <: CoproductK[_], L2[_] <: CoproductK[_]](
    implicit contains: ContainsHK[L2, H], next: SubCop[L, L2]
  ) = new SubCop[ConsK[H, L, ?], L2] {
    def apply[A](l: ConsK[H, L, A]): L2[A] = l match {
      case Inlk(h) => contains.build(h)
      case Inrk(r) => next(r)
    }
  }

}

