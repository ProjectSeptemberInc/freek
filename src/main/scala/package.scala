import scala.language.implicitConversions

import cats.~>
import cats.free.Free

/** a few implicit conversions */
package object freek {

  implicit class ToFreek[F[_], A](val fa: F[A]) extends AnyVal {
    @inline def freek0: Free[ConsK[F, CNilK, ?], A] = Freek(fa)

    @inline def freek[C[_] <: CoproductK[_]](implicit sub: SubCop[ConsK[F, CNilK, ?], C]): Free[C, A] =
      Freek.expand[ConsK[F, CNilK, ?], C, A](freek0)

    @inline def upcast[T](implicit f: F[A] <:< T): T = fa
  }

  implicit class Expand[F[_] <: CoproductK[_], A](val free: Free[F, A]) extends AnyVal {
    @inline def expand[C[_] <: CoproductK[_]](implicit sub: SubCop[F, C]): Free[C, A] =
      Freek.expand[F, C, A](free)
  }

  implicit def toInterpreter[F[_], R[_]](nat: F ~> R): Interpreter[ConsK[F, CNilK, ?], R] = Interpreter(nat)


  type :/:[H[_], T <: FX] = :@:[H, T]

  type :\/:[H[_], T[_] <: CoproductK[_]] = :@@:[H, T]
}
