package freek

import cats.free.Free
import cats.{Applicative, Functor, FlatMap, Monad, Traverse}
import scala.annotation.implicitNotFound


/** Monadic Onion */
sealed trait Onion {
  type Layers[t]
}

sealed trait :&:[H[_], T <: Onion] extends Onion {
  type Layers[t] = H[T#Layers[t]]
}

sealed trait Bulb extends Onion {
  type Layers[t] = t
}

object Onion {

  class Make[O0 <: Onion, L0[_]] {
    type O = O0
    type Layers[t] = L0[t]
  }

  object Make {
    def apply[O0 <: Onion] = new Make[O0, O0#Layers] {}
  }

  def point[S <: Onion, A](a: A)(implicit pointer: Pointer[S]): S#Layers[A] = pointer.point(a)

  def lift[S <: Onion, H[_], A](ha: H[A])(implicit lifter: Lifter[H, S]): S#Layers[A] = lifter.lift(ha)

}


@implicitNotFound("could not prove onion ${S} is pointed; one of the element of the stack might lack an Applicative")
trait Pointer[S <: Onion] {
  def point[A](a: A): S#Layers[A]
}

object Pointer {

  def apply[S <: Onion](implicit pointer: Pointer[S]) = pointer 

  implicit val bulb = new Pointer[Bulb] {
    def point[A](a: A): Bulb#Layers[A] = a
  }

  implicit def cons[H[_]:Applicative, T <: Onion](
    implicit next: Pointer[T]
  ) = new Pointer[H :&: T] {
    def point[A](a: A): (H :&: T)#Layers[A] = Applicative[H].pure(next.point(a))
  }

}

@implicitNotFound("could not lift ${H} into Onion ${S}; either ${S} does not contain a constructor of ${H}, or there is no Applicative Functor for a constructor of ${S}")
trait Lifter[H[_], S <: Onion] {
  def lift[A](ha: H[A]): S#Layers[A]
}

object Lifter extends LifterLow {

  def apply[H[_], S <: Onion](implicit lifter: Lifter[H, S]) = lifter 

  implicit def first2[H[_]:Applicative, I[_], T <: Onion](
    implicit next: Lifter[I, T]
  ): Lifter[λ[t => H[I[t]]], H :&: T] = new Lifter[λ[t => H[I[t]]], H :&: T] {
    def lift[A](hia: H[I[A]]): (H :&: T)#Layers[A] = Functor[H].map(hia)(ia => next.lift(ia))
  }

  implicit def cons2[H[_]:Applicative, I[_], K[_]:Applicative, T <: Onion](
    implicit next: Lifter[λ[t => H[I[t]]], T]
  ): Lifter[λ[t => H[I[t]]], K :&: T] = new Lifter[λ[t => H[I[t]]], K :&: T] {
    def lift[A](hia: H[I[A]]): (K :&: T)#Layers[A] = Applicative[K].pure(next.lift(hia))
  }
}

trait LifterLow {

  // here Applicative isn't needed (just Functor) but you need it to add more elements to onion so...
  implicit def first[H[_]:Applicative, T <: Onion](
    implicit nextPointer: Pointer[T]
  ): Lifter[H, H :&: T] = new Lifter[H, H :&: T] {
    def lift[A](ha: H[A]): (H :&: T)#Layers[A] = Functor[H].map(ha)(a => nextPointer.point(a))
  }

  implicit def cons[H[_]:Functor:Applicative, K[_]:Applicative, T <: Onion](
    implicit next: Lifter[H, T]
  ): Lifter[H, K :&: T] = new Lifter[H, K :&: T] {
    def lift[A](ha: H[A]): (K :&: T)#Layers[A] = Applicative[K].pure(next.lift(ha))
  }

}

@implicitNotFound("could not compute a method for mapping over onion ${S}; either a member of the stack lacks a Functor, or its Functor instance is ambiguous")
trait Mapper[S <: Onion] {
  def map[A, B](fa: S#Layers[A])(f: A => B): S#Layers[B]
}

object Mapper {

  def apply[S <: Onion](implicit mapper: Mapper[S]) = mapper 

  implicit val bulb = new Mapper[Bulb] {
    def map[A, B](fa: Bulb#Layers[A])(f: A => B): Bulb#Layers[B] = f(fa)
  }

  implicit def cons[H[_]:Functor, T <: Onion](
    implicit next: Mapper[T]
  ) = new Mapper[H :&: T] {
    def map[A, B](fa: (H :&: T)#Layers[A])(f: A => B): (H :&: T)#Layers[B] = Functor[H].map(fa)(ta => next.map(ta)(f))
  }
}

@implicitNotFound("could not prove onion ${S} is a valid traversable stack; perhaps an element of the stack is lacking a Traverse")
trait Traverser[S <: Onion] {
  def traverse[G[_]: Applicative, A, B](sa: S#Layers[A])(f: A => G[B]): G[S#Layers[B]]
}

object Traverser {

  def apply[S <: Onion](implicit traverser: Traverser[S]) = traverser 

  implicit val bulb = new Traverser[Bulb] {
    def traverse[G[_]: Applicative, A, B](sa: Bulb#Layers[A])(f: A => G[B]): G[Bulb#Layers[B]] =
      f(sa)
  }

  implicit def cons[H[_]:Traverse, T <: Onion](
    implicit next: Traverser[T]
  ) = new Traverser[H :&: T] {
    def traverse[G[_]: Applicative, A, B](hta: (H :&: T)#Layers[A])(f: A => G[B]): G[(H :&: T)#Layers[B]] =
      Traverse[H].traverse(hta){ ta =>
        next.traverse[G, A, B](ta)(f)
      }
  }
}

@implicitNotFound("could not prove onion ${S} is a valid monadic stack; perhaps an element is lacking a Monad, or one of the sub-onion is lacking a Traverser")
trait Binder[S <: Onion] {
  def bind[A, B](fa: S#Layers[A])(f: A => S#Layers[B]): S#Layers[B]
}

object Binder {

  def apply[S <: Onion](implicit binder: Binder[S]) = binder 

  implicit val nil = new Binder[Bulb] {
    def bind[A, B](fa: Bulb#Layers[A])(f: A => Bulb#Layers[B]): Bulb#Layers[B] = f(fa)
  }

  // H must be a Functor, a FlatMap & an Applicative (for traverse)
  implicit def cons[H[_]:Monad, T <: Onion](
    implicit nextTraverser: Traverser[T], nextBinder: Binder[T]
  ) = new Binder[H :&: T] {
    def bind[A, B](fa: (H :&: T)#Layers[A])(f: A => (H :&: T)#Layers[B]): (H :&: T)#Layers[B] =
      FlatMap[H].flatMap(fa){ ta =>
        val htb: H[T#Layers[T#Layers[B]]] = nextTraverser.traverse(ta){ a => f(a) }
        FlatMap[H].map(htb){ ttb => nextBinder.bind(ttb){ tb => tb } }
      }
  }
}


@implicitNotFound("could not expand Onion ${S1} into Onion ${S2}; either ${S1} must be a sub-onion of ${S2}")
trait Expander[S1 <: Onion, S2 <: Onion] {
  def expand[A](s1: S1#Layers[A]): S2#Layers[A]
}

object Expander {

  def apply[S1 <: Onion, S2 <: Onion](implicit expander: Expander[S1, S2]) = expander

  implicit val bulb = new Expander[Bulb, Bulb] {
    def expand[A](s1a: Bulb#Layers[A]): Bulb#Layers[A] = s1a
  } 

  // H must be a Functor, a FlatMap & an Applicative (for traverse)
  implicit def first[H[_]:Functor, S1 <: Onion, S2 <: Onion](
    implicit next: Expander[S1, S2]
  ) = new Expander[H :&: S1, H :&: S2] {
    def expand[A](sS1a: (H :&: S1)#Layers[A]): (H :&: S2)#Layers[A] =
      Functor[H].map(sS1a){ S1a => next.expand(S1a) }
  }

  // H must be a Functor, a FlatMap & an Applicative (for traverse)
  implicit def cons[H[_]:Applicative, S1 <: Onion, S2 <: Onion](
    implicit next: Expander[S1, S2]
  ) = new Expander[S1, H :&: S2] {
    def expand[A](hs1a: S1#Layers[A]): (H :&: S2)#Layers[A] =      
      Applicative[H].pure(next.expand(hs1a))
  }
}



trait PeelRight[S <: Onion] {
  type OutS <: Onion
  type Out[_]

  def peelRight[A](s: S#Layers[A]): OutS#Layers[Out[A]]
}

object PeelRight {

  type Aux[S <: Onion, OutS0 <: Onion, Out0[_]] = PeelRight[S] { type OutS = OutS0; type Out[t] = Out0[t] }

  def apply[S <: Onion](implicit dr: PeelRight[S]) = dr

  implicit def first[H[_]]: PeelRight.Aux[H :&: Bulb, Bulb, H] = new PeelRight[H :&: Bulb] {
    type OutS = Bulb
    type Out[t] = H[t]

    def peelRight[A](hka: (H :&: Bulb)#Layers[A]): Bulb#Layers[H[A]] = {
      hka
    }
  }

  implicit def cons[H[_]:Functor, S <: Onion, NextS <: Onion, Next[_]](
    implicit next: PeelRight.Aux[S, NextS, Next]
  ): PeelRight.Aux[H :&: S, H :&: NextS, Next] = new PeelRight[H :&: S] {
    type OutS = H :&: NextS
    type Out[t] = Next[t]

    def peelRight[A](hsa: (H :&: S)#Layers[A]): (H :&: NextS)#Layers[Next[A]] =
      Functor[H].map(hsa){ sa => next.peelRight(sa) }
  }
}


trait PeelRight2[S <: Onion] {
  type OutS <: Onion
  type Out[_]

  def peelRight[A](s: S#Layers[A]): OutS#Layers[Out[A]]
}


object PeelRight2 {

  type Aux[S <: Onion, OutS0 <: Onion, Out0[_]] = PeelRight2[S] { type OutS = OutS0; type Out[t] = Out0[t] }

  def apply[S <: Onion](implicit dr: PeelRight[S]) = dr

  implicit def two[S <: Onion, OutS1 <: Onion, Out1[_], OutS2 <: Onion, Out2[_]](
    implicit
      peel1: PeelRight.Aux[S, OutS1, Out1]
    , peel2: PeelRight.Aux[OutS1, OutS2, Out2]
  ): PeelRight2.Aux[S, OutS2, λ[t => Out2[Out1[t]]]] = new PeelRight2[S] {
    type OutS = OutS2
    type Out[t] = Out2[Out1[t]]

    def peelRight[A](hka: S#Layers[A]): OutS2#Layers[Out2[Out1[A]]] = {
      peel2.peelRight(peel1.peelRight(hka))
    }
  }

}

trait PeelRight3[S <: Onion] {
  type OutS <: Onion
  type Out[_]

  def peelRight[A](s: S#Layers[A]): OutS#Layers[Out[A]]
}


object PeelRight3 {

  type Aux[S <: Onion, OutS0 <: Onion, Out0[_]] = PeelRight3[S] { type OutS = OutS0; type Out[t] = Out0[t] }

  def apply[S <: Onion](implicit dr: PeelRight[S]) = dr

  implicit def three[S <: Onion, OutS1 <: Onion, Out1[_], OutS2 <: Onion, Out2[_], OutS3 <: Onion, Out3[_]](
    implicit
      peel1: PeelRight.Aux[S, OutS1, Out1]
    , peel2: PeelRight.Aux[OutS1, OutS2, Out2]
    , peel3: PeelRight.Aux[OutS2, OutS3, Out3]
  ): PeelRight3.Aux[S, OutS3, λ[t => Out3[Out2[Out1[t]]]]] = new PeelRight3[S] {
    type OutS = OutS3
    type Out[t] = Out3[Out2[Out1[t]]]

    def peelRight[A](hka: S#Layers[A]): OutS3#Layers[Out3[Out2[Out1[A]]]] = {
      peel3.peelRight(peel2.peelRight(peel1.peelRight(hka)))
    }
  }

}

trait Wrap[H[_], S <: Onion] {
  type Out <: Onion

  def wrap[A](s: S#Layers[A]): Out#Layers[A]
}


object Wrap {

  def apply[H[_], S <: Onion](implicit Wrap: Wrap[H, S]) = Wrap

  implicit def Layers[H[_]: Applicative, S <: Onion] = new Wrap[H, S] {
    type Out = H :&: S
    def wrap[A](s: S#Layers[A]): (H :&: S)#Layers[A] = Applicative[H].pure(s)
  }
}

@implicitNotFound("could not decompose ${FA} into a stack of type containers F[G[...X[A]]]")
trait HKK[FA] {
  type A
}

object HKK extends HKK3 {

  type Aux[FA, A0] = HKK[FA] { type A = A0 }

}

trait HKK3 extends HKK2 {
  implicit def hk3[F[_], G[_], H[_], A0]: HKK.Aux[F[G[H[A0]]], A0] = new HKK[F[G[H[A0]]]] {
    type A = A0
  }
}

trait HKK2 extends HKK1 {
  implicit def hk2[F[_], G[_], A0]: HKK.Aux[F[G[A0]], A0] = new HKK[F[G[A0]]] {
    type A = A0
  }
}

trait HKK1 {
  implicit def hk1[F[_], A0]: HKK.Aux[F[A0], A0] = new HKK[F[A0]] {
    type A = A0
  }
}

@implicitNotFound("could not lift2 ${HA} into Onion ${S}; either ${S} does not contain a constructor of ${HA}, or there is no Applicative Functor for a constructor of ${S}")
trait Lifter2[HA, S <: Onion] {
  type A
  def lift2(ha: HA): S#Layers[A]
}

object Lifter2 extends Lifter2Low {
  type Aux[HA, S <: Onion, A0] = Lifter2[HA, S] { type A = A0 }

  def apply[HA, S <: Onion](implicit lifter: Lifter2[HA, S]) = lifter 

  implicit def cons2[H[_]:Applicative, IA, T <: Onion](
    implicit next: Lifter2[IA, T]
  ): Lifter2.Aux[H[IA], H :&: T, next.A] = new Lifter2[H[IA], H :&: T] {
    type A = next.A
    def lift2(hia: H[IA]): (H :&: T)#Layers[A] = Functor[H].map(hia)(ia => next.lift2(ia))
  }
}

trait Lifter2Low {
  implicit def first[H[_]:Applicative, A0, T <: Onion](
    implicit nextPointer: Pointer[T]
  ): Lifter2.Aux[H[A0], H :&: T, A0] = new Lifter2[H[A0], H :&: T] {
    type A = A0
    def lift2(ha: H[A0]): (H :&: T)#Layers[A] = Functor[H].map(ha)(a => nextPointer.point(a))
  }

  implicit def cons[H[_]:Functor:Applicative, A0, K[_]:Applicative, T <: Onion](
    implicit next: Lifter2[H[A0], T]
  ): Lifter2.Aux[H[A0], K :&: T, next.A] = new Lifter2[H[A0], K :&: T] {
    type A = next.A
    def lift2(ha: H[A0]): (K :&: T)#Layers[A] = Applicative[K].pure(next.lift2(ha))
  }


}