package freek


/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  */
import org.scalatest._

import cats.free.{Free, Trampoline}
import cats.data.Xor
import cats.{~>, Id}

import scala.concurrent._
import scala.concurrent.duration._

// import cats.derived._, functor._, legacy._
import cats.Functor
import cats.std.future._
import cats.std.option._
import cats.std.list._
import ExecutionContext.Implicits.global

import freek._


class LongCompileSpec extends FlatSpec with Matchers {

  /** weird this is not provided in cats apparently */
  implicit val fc = new cats.Comonad[Function0] {
    def extract[A](x: () => A): A = x()
    def coflatMap[A, B](fa: () => A)(f: (() => A) => B): () => B = () => f(fa)
    def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())
  }

  case class Foo1[A](a: A)
  case class Foo2[A](a: A)
  case class Foo3[A](a: A)
  case class Foo4[A](a: A)
  case class Foo5[A](a: A)
  case class Foo6[A](a: A)
  case class Foo7[A](a: A)
  case class Foo8[A](a: A)
  case class Foo9[A](a: A)
  case class Foo10[A](a: A)
  case class Foo11[A](a: A)
  case class Foo12[A](a: A)
  case class Foo13[A](a: A)
  case class Foo14[A](a: A)
  case class Foo15[A](a: A)
  case class Foo16[A](a: A)
  case class Foo17[A](a: A)
  case class Foo18[A](a: A)
  case class Foo19[A](a: A)
  case class Foo20[A](a: A)

  "Freek" should "long compile" in {

    type PRG =
      Foo1 :|: Foo2 :|: Foo3 :|: Foo4 :|: Foo5 :|: Foo6 :|: Foo7 :|: Foo8 :|: Foo9 :|: Foo10 :|: 
      Foo11 :|: Foo12 :|: Foo13 :|: Foo14 :|: Foo15 :|: Foo16 :|: Foo17 :|: Foo18 :|: Foo19 :|: Foo20 :|:
      NilDSL

    val PRG = DSL.Make[PRG]


    val prg: Free[PRG.Cop, Int] = for {
      a <- Foo1(5).freek[PRG]
      a <- Foo2(a).freek[PRG]
      a <- Foo3(a).freek[PRG]
      a <- Foo4(a).freek[PRG]
      a <- Foo5(a).freek[PRG]
      a <- Foo6(a).freek[PRG]
      a <- Foo7(a).freek[PRG]
      a <- Foo8(a).freek[PRG]
      a <- Foo9(a).freek[PRG]
      a <- Foo10(a).freek[PRG]
      a <- Foo11(a).freek[PRG]
      a <- Foo12(a).freek[PRG]
      a <- Foo13(a).freek[PRG]
      a <- Foo14(a).freek[PRG]
      a <- Foo15(a).freek[PRG]
      a <- Foo16(a).freek[PRG]
      a <- Foo17(a).freek[PRG]
      a <- Foo18(a).freek[PRG]
      a <- Foo19(a).freek[PRG]
      a <- Foo20(a).freek[PRG]
    } yield (a)

    object Foo1I extends (Foo1 ~> cats.Id) {
      def apply[A](a: Foo1[A]) = a match {
        case Foo1(a) => a
      }
    }

    object Foo2I extends (Foo2 ~> cats.Id) {
      def apply[A](a: Foo2[A]) = a match {
        case Foo2(a) => a
      }
    }

    object Foo3I extends (Foo3 ~> cats.Id) {
      def apply[A](a: Foo3[A]) = a match {
        case Foo3(a) => a
      }
    }

    object Foo4I extends (Foo4 ~> cats.Id) {
      def apply[A](a: Foo4[A]) = a match {
        case Foo4(a) => a
      }
    }

    object Foo5I extends (Foo5 ~> cats.Id) {
      def apply[A](a: Foo5[A]) = a match {
        case Foo5(a) => a
      }
    }

    object Foo6I extends (Foo6 ~> cats.Id) {
      def apply[A](a: Foo6[A]) = a match {
        case Foo6(a) => a
      }
    }

    object Foo7I extends (Foo7 ~> cats.Id) {
      def apply[A](a: Foo7[A]) = a match {
        case Foo7(a) => a
      }
    }

    object Foo8I extends (Foo8 ~> cats.Id) {
      def apply[A](a: Foo8[A]) = a match {
        case Foo8(a) => a
      }
    }        

    object Foo9I extends (Foo9 ~> cats.Id) {
      def apply[A](a: Foo9[A]) = a match {
        case Foo9(a) => a
      }
    }

    object Foo10I extends (Foo10 ~> cats.Id) {
      def apply[A](a: Foo10[A]) = a match {
        case Foo10(a) => a
      }
    }

    object Foo11I extends (Foo11 ~> cats.Id) {
      def apply[A](a: Foo11[A]) = a match {
        case Foo11(a) => a
      }
    }

    object Foo12I extends (Foo12 ~> cats.Id) {
      def apply[A](a: Foo12[A]) = a match {
        case Foo12(a) => a
      }
    }

    object Foo13I extends (Foo13 ~> cats.Id) {
      def apply[A](a: Foo13[A]) = a match {
        case Foo13(a) => a
      }
    }

    object Foo14I extends (Foo14 ~> cats.Id) {
      def apply[A](a: Foo14[A]) = a match {
        case Foo14(a) => a
      }
    }

    object Foo15I extends (Foo15 ~> cats.Id) {
      def apply[A](a: Foo15[A]) = a match {
        case Foo15(a) => a
      }
    }

    object Foo16I extends (Foo16 ~> cats.Id) {
      def apply[A](a: Foo16[A]) = a match {
        case Foo16(a) => a
      }
    }

    object Foo17I extends (Foo17 ~> cats.Id) {
      def apply[A](a: Foo17[A]) = a match {
        case Foo17(a) => a
      }
    }

    object Foo18I extends (Foo18 ~> cats.Id) {
      def apply[A](a: Foo18[A]) = a match {
        case Foo18(a) => a
      }
    }

    object Foo19I extends (Foo19 ~> cats.Id) {
      def apply[A](a: Foo19[A]) = a match {
        case Foo19(a) => a
      }
    }

    object Foo20I extends (Foo20 ~> cats.Id) {
      def apply[A](a: Foo20[A]) = a match {
        case Foo20(a) => a
      }
    }

    val interpreter =
      Foo1I :&: Foo2I :&: Foo3I :&: Foo4I :&: Foo5I :&: Foo6I :&: Foo7I :&: Foo8I :&: Foo9I :&: Foo10I :&:
      Foo11I :&: Foo12I :&: Foo13I :&: Foo14I :&: Foo15I :&: Foo16I :&: Foo17I :&: Foo18I :&: Foo19I :&: Foo20I

    val res = prg.interpret(interpreter)
    println(s"Res: $res")
  }
}
