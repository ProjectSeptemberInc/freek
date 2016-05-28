package freek

import cats.free.Free
import cats.{Applicative, Functor, FlatMap, Monad, Traverse}
import scala.annotation.implicitNotFound


/** Monadic Onion */
sealed trait Onion {
  type Build[t]
}

sealed trait :&:[H[_], T <: Onion] extends Onion {
  type Build[t] = H[T#Build[t]]
}

sealed trait SNil extends Onion {
  type Build[t] = t
}

object Onion {

  def point[S <: Onion, A](a: A)(implicit pointer: Pointer[S]): S#Build[A] = pointer.point(a)

  def lift[S <: Onion, H[_], A](ha: H[A])(implicit lifter: Lifter[H, S]): S#Build[A] = lifter.lift(ha)

}


@implicitNotFound("could not prove onion ${S} is pointed; one of the element of the stack might lack an Applicative")
trait Pointer[S <: Onion] {
  def point[A](a: A): S#Build[A]
}

object Pointer {

  implicit def nil = new Pointer[SNil] {
    def point[A](a: A): SNil#Build[A] = a
  }

  implicit def cons[H[_]:Applicative, T <: Onion](
    implicit next: Pointer[T]
  ) = new Pointer[H :&: T] {
    def point[A](a: A): (H :&: T)#Build[A] = Applicative[H].pure(next.point(a))
  }

}

@implicitNotFound("could not lift ${H} into Onion ${S}; either ${S} does not contain a constructor of ${H}, or there is no Applicative Functor for a constructor of ${S}")
trait Lifter[H[_], S <: Onion] {
  def lift[A](ha: H[A]): S#Build[A]
}

object Lifter {

  def apply[H[_], S <: Onion](implicit lifter: Lifter[H, S]) = lifter 

  // here Applicative isn't needed (just Functor) but you need it to add more elements to onion so...
  implicit def first[H[_]:Applicative, T <: Onion](
    implicit nextPointer: Pointer[T]
  ): Lifter[H, H :&: T] = new Lifter[H, H :&: T] {
    def lift[A](ha: H[A]): (H :&: T)#Build[A] = Functor[H].map(ha)(a => nextPointer.point(a))
  }

  implicit def cons[H[_]:Functor:Applicative, K[_]:Applicative, T <: Onion](
    implicit next: Lifter[H, T]
  ): Lifter[H, K :&: T] = new Lifter[H, K :&: T] {
    def lift[A](ha: H[A]): (K :&: T)#Build[A] = Applicative[K].pure(next.lift(ha))
  }  
}


@implicitNotFound("could not compute a method for mapping over onion ${S}; either a member of the stack lacks a Functor, or its Functor instance is ambiguous")
trait Mapper[S <: Onion] {
  def map[A, B](fa: S#Build[A])(f: A => B): S#Build[B]
}

object Mapper {
  implicit def nil = new Mapper[SNil] {
    def map[A, B](fa: SNil#Build[A])(f: A => B): SNil#Build[B] = f(fa)
  }

  implicit def cons[H[_]:Functor, T <: Onion](
    implicit next: Mapper[T]
  ) = new Mapper[H :&: T] {
    def map[A, B](fa: (H :&: T)#Build[A])(f: A => B): (H :&: T)#Build[B] = Functor[H].map(fa)(ta => next.map(ta)(f))
  }
}

@implicitNotFound("could not prove onion ${S} is a valid traversable stack; perhaps an element of the stack is lacking a Traverse")
trait Traverser[S <: Onion] {
  def traverse[G[_]: Applicative, A, B](sa: S#Build[A])(f: A => G[B]): G[S#Build[B]]
}

object Traverser {

  implicit def nil = new Traverser[SNil] {
    def traverse[G[_]: Applicative, A, B](sa: SNil#Build[A])(f: A => G[B]): G[SNil#Build[B]] =
      f(sa)
  }

  implicit def cons[H[_]:Traverse, T <: Onion](
    implicit next: Traverser[T]
  ) = new Traverser[H :&: T] {
    def traverse[G[_]: Applicative, A, B](hta: (H :&: T)#Build[A])(f: A => G[B]): G[(H :&: T)#Build[B]] =
      Traverse[H].traverse(hta){ ta =>
        next.traverse[G, A, B](ta)(f)
      }
  }
}

@implicitNotFound("could not prove onion ${S} is a valid monadic stack; perhaps an element is lacking a Monad, or one of the sub-onion is lacking a Traverser")
trait Binder[S <: Onion] {
  def bind[A, B](fa: S#Build[A])(f: A => S#Build[B]): S#Build[B]
}

object Binder {
  implicit def nil = new Binder[SNil] {
    def bind[A, B](fa: SNil#Build[A])(f: A => SNil#Build[B]): SNil#Build[B] = f(fa)
  }

  // H must be a Functor, a FlatMap & an Applicative (for traverse)
  implicit def cons[H[_]:Monad, T <: Onion](
    implicit nextTraverser: Traverser[T], nextBinder: Binder[T]
  ) = new Binder[H :&: T] {
    def bind[A, B](fa: (H :&: T)#Build[A])(f: A => (H :&: T)#Build[B]): (H :&: T)#Build[B] =
      FlatMap[H].flatMap(fa){ ta =>
        val htb: H[T#Build[T#Build[B]]] = nextTraverser.traverse(ta){ a => f(a) }
        FlatMap[H].map(htb){ ttb => nextBinder.bind(ttb){ tb => tb } }
      }
  }
}


