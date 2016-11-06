package freek

import scala.language.higherKinds

import cats.data.{ OptionT, EitherT }
import cats.Functor
import cats.free._


trait HasHoist[M[_]] {
  type T[_[_], _]
  def liftT[F[_], A](f: F[M[A]]): T[F, A]
  def liftF[F[_] : Functor, A](f: F[A]): T[F, A]
}

class EitherHasHoist[A] extends HasHoist[λ[t => Either[A, t]]] {
  type T[F[_], B] = EitherT[F, A, B]
  def liftT[F[_], B](f: F[Either[A, B]]): EitherT[F, A, B] = EitherT.apply(f)
  def liftF[F[_] : Functor, B](f: F[B]): EitherT[F, A, B] = EitherT.right(f)
}

object HasHoist {
  type Aux[M[_], T0[_[_], _]] = HasHoist[M] { type T[F[_], A] = T0[F, A] }

  def apply[M[_]](implicit h: HasHoist[M]): Aux[M, h.T] = h

  implicit val optionHasHoist: HasHoist.Aux[Option, OptionT] = new HasHoist[Option] {
    type T[F[_], A] = OptionT[F, A]
    def liftT[F[_], A](f: F[Option[A]]): OptionT[F, A] = OptionT(f)
    def liftF[F[_] : Functor, B](f: F[B]): OptionT[F, B] = OptionT.liftF(f)
  }

  implicit def eitherHasHoist[A]: HasHoist.Aux[λ[t => Either[A, t]], λ[(f[_], b) => EitherT[f, A, b]]] =
    new EitherHasHoist[A]

}

/**
  * Helpers for Precepte wrapped in Monad Transformers (OptionT, ListT, EitherT)
  */
trait HK {

  implicit class toTrans1[F[_], G[_], A](m: F[G[A]]) {

    def liftT[G0[_]](implicit hh: HasHoist[G0], witness: F[G[A]] =:= F[G0[A]]): hh.T[F, A] =
      hh.liftT[F, A](witness(m))

  }


  implicit class toTrans2[F[_] : Functor, A](m: F[A]) {

    def liftF[G[_]](implicit hh: HasHoist[G]): hh.T[F, A] =
      hh.liftF[F, A](m)

  }


}

package object hk extends HK