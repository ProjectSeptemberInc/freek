import scala.language.implicitConversions

import cats.~>


/** a few implicit conversions */
package object freek {

  implicit def uplist[C[_] <: CoproductK[_], A, D[_] <: CoproductK[_]](freek: Freek[C, A])(
    implicit subCop: SubCop[C, D]
  ): Freek[D, A] = {

    def nat[F[_] <: CoproductK[_], G[_] <: CoproductK[_]](implicit subCop: SubCop[F, G]) = new (F ~> G) {
      def apply[A](fa: F[A]): G[A] = subCop(fa)
    }

    new Freek(freek.free.mapSuspension(nat))
  }

  implicit class ToFreek[F[_], A](val fa: F[A]) extends AnyVal {
    def freek: Freek[ConsK[F, CNilK, ?], A] = Freek(fa)
  }

  implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[ConsK[F, CNilK, ?], R] = Interpreter(nat)

}