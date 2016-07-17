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

  "Freek" should "long compile" in {

    // type PRG1 = Foo1 :|: Foo2 :|: Foo3 :|: Foo4 :|: FXNil
    // type PRG2 = Foo5 :|: /*Foo6 :|: Foo7 :|: Foo8 :|: Foo9 :|:*/ FXNil
    // type PRG = PRG1 :||: PRG2
    type PRG = Foo1 :|: Foo2 :|: Foo3 :|: Foo4 :|: Foo5 :|: /*Foo6 :|: Foo7 :|: Foo8 :|: Foo9 :|:*/ FXNil

    val prg = for {
      a <- Foo1(5).freek[PRG]
      a <- Foo2(a).freek[PRG]
      a <- Foo3(a).freek[PRG]
      a <- Foo4(a).freek[PRG]
      a <- Foo5(a).freek[PRG]
      // a <- Foo6(a).freek[PRG]
      // a <- Foo7(a).freek[PRG]
      // a <- Foo8(a).freek[PRG]
      // a <- Foo9(a).freek[PRG]
    } yield (a)

    // object Foo1I extends (Foo1 ~> cats.Id) {
    //   def apply[A](a: Foo1[A]) = a match {
    //     case Foo1(a) => a
    //   }
    // }

    // object Foo2I extends (Foo2 ~> cats.Id) {
    //   def apply[A](a: Foo2[A]) = a match {
    //     case Foo2(a) => a
    //   }
    // }

    // object Foo3I extends (Foo3 ~> cats.Id) {
    //   def apply[A](a: Foo3[A]) = a match {
    //     case Foo3(a) => a
    //   }
    // }

    // object Foo4I extends (Foo4 ~> cats.Id) {
    //   def apply[A](a: Foo4[A]) = a match {
    //     case Foo4(a) => a
    //   }
    // }

    // object Foo5I extends (Foo5 ~> cats.Id) {
    //   def apply[A](a: Foo5[A]) = a match {
    //     case Foo5(a) => a
    //   }
    // }

    // object Foo6I extends (Foo6 ~> cats.Id) {
    //   def apply[A](a: Foo6[A]) = a match {
    //     case Foo6(a) => a
    //   }
    // }

    // object Foo7I extends (Foo7 ~> cats.Id) {
    //   def apply[A](a: Foo7[A]) = a match {
    //     case Foo7(a) => a
    //   }
    // }

    // object Foo8I extends (Foo8 ~> cats.Id) {
    //   def apply[A](a: Foo8[A]) = a match {
    //     case Foo8(a) => a
    //   }
    // }        

    // object Foo9I extends (Foo9 ~> cats.Id) {
    //   def apply[A](a: Foo9[A]) = a match {
    //     case Foo9(a) => a
    //   }
    // }

    // val interpreter = Foo1I :&: Foo2I :&: Foo3I :&: Foo4I :&: Foo5I :&: Foo6I :&: Foo7I :&: Foo8I :&: Foo9I
    // val res = prg.interpret(interpreter)
    // println(s"Res: $res")
  }
}