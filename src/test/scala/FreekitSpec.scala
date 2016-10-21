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
import cats.instances.future._
import cats.instances.option._
import cats.instances.list._
import ExecutionContext.Implicits.global

import freek._


class FreekitSpec extends FlatSpec with Matchers {

  "Freek" should "macro" in {

    //////////////////////////////////////////////////////////////////////////
    // LOG DSL
    sealed trait Log[A]
    object Log {
      case class Info(msg: String) extends Log[Unit]
    }

    sealed trait Foo1[A]
    object Foo1 {
      final case class Bar1(a: Int) extends Foo1[Option[Int]]
    }

    sealed trait Foo2[A]
    object Foo2 {
      final case class Bar21(a: Int) extends Foo2[Int]
      final case object Bar22 extends Foo2[Int]
    }


    type PRG = Foo1 :|: Foo2 :|: Log :|: NilDSL
    val PRG = DSL.Make[PRG]

    object M extends Freekit(PRG) {
      val prg = for {
        aOpt <- Foo1.Bar1(7)
        _    <- Log.Info(s"aOpt:$aOpt")
        a    <- aOpt match {
          case Some(a)  =>  for {
                              a <- Foo2.Bar21(a)
                              _ <- Log.Info(s"a1:$a")
                            } yield (a)
          case None     =>  for {
                              a <- Foo2.Bar22
                              _ <- Log.Info(s"a2:$a")
                            } yield (a)
        }
      } yield (a)
    }

    object MO extends Freekito(PRG) {
      type O = Option :&: Bulb

      val prg = for {
        a    <- Foo1.Bar1(7)
        _    <- Log.Info(s"a:$a")
        a    <- Foo2.Bar21(a)
      } yield (a)
    }

    val foo1I = new (Foo1 ~> Future) {
      import Foo1._

      def apply[A](f: Foo1[A]): Future[A] = f match {
        case Bar1(a) => Future(Some(a))

      }
    }

    val foo2I = new (Foo2 ~> Future) {
      import Foo2._

      def apply[A](f: Foo2[A]): Future[A] = f match {
        case Bar21(a) => Future(a)
        case Bar22 => Future(0)        
      }
    }


    val logI = new (Log ~> Future) {
      def apply[A](a: Log[A]) = a match {
        case Log.Info(msg) =>
          Future.successful(println(s"[info] $msg"))
      }
    }

    val f = M.prg.interpret(foo1I :&: foo2I :&: logI)
    Await.result(f, 10.seconds)

    val f2 = MO.prg.value.interpret(foo1I :&: foo2I :&: logI)
    Await.result(f2, 10.seconds)
  }


  "Freekit" should "special cases 4" in {
    sealed trait Foo1[A]
    final case class Bar11(s: Int) extends Foo1[Xor[String, List[Int]]]
    final case class Bar12(s: List[Int]) extends Foo1[Xor[String, Option[Int]]]

    sealed trait Foo2[A]
    final case class Bar21(s: Int) extends Foo1[Xor[Long, Option[List[Int]]]]
    final case class Bar22(s: List[Int]) extends Foo1[Xor[Long, Option[Int]]]

    type PRG = Foo1 :|: Foo2 :|: NilDSL
    val PRG = DSL.Make[PRG]

    object F1 extends Freekito(PRG) {
      type O = Xor[String, ?] :&: Xor[Long, ?] :&: Option :&: Bulb

      val prg = for {
        l1 <- Bar11(5).freek[PRG].onionX1[O]
        _  <- Bar12(l1)
        l2 <- Bar21(6).freek[PRG].onionX2[O]
        _  <- Bar22(l2)
      } yield (())
    }
  }

  "Freekit" should "freek" in {
    import Http._
    import DB._

    object DBService extends Freekit(DSL.Make[Log.DSL :|: DB.DSL :|: NilDSL]) {

      /** the DSL.Make */
      def findById(id: String): Free[PRG.Cop, Xor[DBError, Entity]] =
        for {
          _    <- Log.debug("Searching for entity id:"+id)
          res  <- FindById(id)
          _    <- Log.debug("Search result:"+res)
        } yield (res)
    }

    object HttpService extends Freekit(DSL.Make[Log.DSL :|: HttpInteract :|: HttpHandle :|: DBService.PRG]) {

      def handle(req: HttpReq): Free[PRG.Cop, HttpResp] = req.url match {
        case "/foo" =>
          for {
            _     <-  Log.debug("/foo")
            dbRes <-  DBService.findById("foo").expand[PRG]

            resp  <-  HttpHandle.result(
                        dbRes match {
                          case Xor.Left(err) => HttpResp(status = InternalServerError)
                          case Xor.Right(e)   => HttpResp(status = Ok, body = e.toString)
                        }
                      )
          } yield (resp)

        case _ => HttpHandle.result(HttpResp(status = InternalServerError))
      }

      def serve() : Free[PRG.Cop, Xor[RecvError, SendStatus]] =
        for {
          recv  <-  HttpInteract.receive()
          _     <-  Log.info("HttpReceived Request:"+recv)
          res   <-  recv match {
                      case Xor.Left(err) => HttpInteract.stop(Xor.left(err)).freek[PRG]

                      case Xor.Right(req) =>
                        for {
                          resp  <-  handle(req)
                          _     <-  Log.info("Sending Response:"+resp)
                          ack   <-  HttpInteract.respond(resp)
                          res   <-  if(ack == Ack) serve()
                                    else HttpInteract.stop(Xor.right(ack)).freek[PRG]
                        } yield (res)
                    }
        } yield (res)

    }
  }

}