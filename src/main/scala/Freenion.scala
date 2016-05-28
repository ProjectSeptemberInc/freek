package freek

import cats.free.Free
import cats.{Applicative, Functor, FlatMap, Monad, Traverse}


/** A Free wrapper to manipulate monadic stack of results */
case class Freenion[F[_], S <: Onion, A](free: Free[F, S#Build[A]]) extends AnyVal {

  def map[B](f: A => B)(implicit mapper: Mapper[S]): Freenion[F, S, B] =
    Freenion(free.map{ a => mapper.map(a)(f) })

  def flatMap[B](f: A => Freenion[F, S, B])(implicit binder: Binder[S], traverser: Traverser[S]): Freenion[F, S, B] =
    Freenion(
      free.flatMap{ sba: S#Build[A] =>
        val freesbb = traverser.traverse(sba){ a => f(a).free }
        freesbb.map { sbb => binder.bind(sbb){ sb => sb } }
      }
    )

}

object Freenion {

  def onion[C[_], S <: Onion, A](fa: Free[C, A])(implicit pointer: Pointer[S]): Freenion[C, S, A] =
    Freenion(fa.map (a => pointer.point(a)))

  def onionF[C[_], S <: Onion, F[_], A](fa: Free[C, F[A]])(implicit lifter: Lifter[F, S]): Freenion[C, S, A] =
    Freenion(fa.map (fa => lifter.lift(fa)))

}

trait FreenionHelpers {

  implicit class toOnionF[F[_], G[_], A](free: Free[F, G[A]]) {

    def onionF[S <: Onion](implicit lifter: Lifter[G, S]): Freenion[F, S, A] =
      Freenion.onionF(free)

  }

  implicit class toOnion[F[_], G[_], A](free: Free[F, A]) {

    def onion[S <: Onion](implicit pointer: Pointer[S]): Freenion[F, S, A] =
      Freenion.onion(free)

  }

}