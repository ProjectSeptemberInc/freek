package freek

import scala.language.higherKinds

import cats.data.{ OptionT, XorT, Xor }
import cats.Functor
import cats.free._


trait HasHoist[M[_]] {
  type T[_[_], _]
  def liftT[F[_], A](f: F[M[A]]): T[F, A]
  def liftF[F[_] : Functor, A](f: F[A]): T[F, A]
}

class XorHasHoist[A] extends HasHoist[λ[t => Xor[A, t]]] {
  type T[F[_], B] = XorT[F, A, B]
  def liftT[F[_], B](f: F[Xor[A, B]]): XorT[F, A, B] = XorT.apply(f)
  def liftF[F[_] : Functor, B](f: F[B]): XorT[F, A, B] = XorT.right(f)
}

object HasHoist {
  type Aux[M[_], T0[_[_], _]] = HasHoist[M] { type T[F[_], A] = T0[F, A] }

  def apply[M[_]](implicit h: HasHoist[M]): Aux[M, h.T] = h

  implicit val optionHasHoist: HasHoist.Aux[Option, OptionT] = new HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def liftT[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT(f)
    def liftF[F[_] : Functor, B](f: F[B]): OptionT[F, B] = OptionT.liftF(f)
  }

  implicit def xorHasHoist[A]: HasHoist.Aux[λ[t => Xor[A, t]], λ[(f[_], b) => XorT[f, A, b]]] =
    new XorHasHoist[A]

}

/**
  * Helpers for Precepte wrapped in Monad Transformers (OptionT, ListT, EitherT)
  */
trait HK {

  /**
    * Finds the right Monad Transformer (OptionT, ListT, EitherT) wrapped in a precepte using HasHoist typeclass
    * and lifts the Precepte into a MonadTransformer instance. 
    */
  // implicit class toTrans[C[_] <: CoproductK[_], G[_], A](m: Free[C, G[A]]) {

  //   def liftT(implicit hh: HasHoist[G]): hh.T[λ[t => Free[C, t]], A] =
  //     hh.lift[λ[t => Free[C, t]], A](m)

  // }

  // implicit class toTrans0[C[_] <: CoproductK[_], A](m: Free[C, A]) {

  //   def liftT[G[_]](implicit hh: HasHoist[G]): hh.T[λ[t => Free[C, t]], A] =
  //     hh.lift[λ[t => Free[C, t]], A](m)

  // }

  implicit class toTrans1[F[_], G[_], A](m: F[G[A]]) {

    def liftT[G0[_]](implicit hh: HasHoist[G0], witness: F[G[A]] =:= F[G0[A]]): hh.T[F, A] =
      hh.liftT[F, A](witness(m))

  }


  implicit class toTrans2[F[_] : Functor, A](m: F[A]) {

    def liftF[G[_]](implicit hh: HasHoist[G]): hh.T[F, A] =
      hh.liftF[F, A](m)

  }

  // def trans2[C[_] <: CoproductK[_], G[_, _], A, B](
  //   m: Free[C, G[A, B]]
  // )(implicit hh: HasHoist[λ[t => G[A, t]]]): hh.T[λ[t => Free[C, t]], B] = {
  //   type λ[α] = G[A, α]
  //   trans(m)
  // }

}

package object hk extends HK