import scala.language.implicitConversions

import cats.~>
import cats.free.Free

/** a few implicit conversions */
package object freek {

  implicit class ToFreek[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def freek0: Free[ConsK[F, CNilK, ?], A] = Freek(fa)

    @inline def freek[C[_] <: CoproductK[_]](implicit sub: SubCop[ConsK[F, CNilK, ?], C]): Free[C, A] =
      Freek.expand[ConsK[F, CNilK, ?], C, A](freek0)
  }

  implicit class Expand[F[_] <: CoproductK[_], A](val free: Free[F, A]) extends AnyVal {
    @inline def expand[C[_] <: CoproductK[_]](implicit sub: SubCop[F, C]): Free[C, A] =
      Freek.expand[F, C, A](free)
  }

  implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[ConsK[F, CNilK, ?], R] = Interpreter(nat)

}


  // implicit def uplist[C[_] <: CoproductK[_], A, D[_] <: CoproductK[_]](freek: Freek[C, A])(
  //   implicit subCop: SubCop[C, D]
  // ): Freek[D, A] = {

  //   val nat = new (C ~> D) {
  //     def apply[A](fa: C[A]): D[A] = subCop(fa)
  //   }

  //   new Freek(freek.free.mapSuspension(nat))
  // }
