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


//////////////////////////////////////////////////////////////////////////
// Declare DSLs

//////////////////////////////////////////////////////////////////////////
// LOG DSL
object Log {
  sealed trait LogLevel
  case object ErrorLevel extends LogLevel
  case object WarnLevel extends LogLevel
  case object InfoLevel extends LogLevel
  case object DebugLevel extends LogLevel

  trait DSL[A]
  case class LogMsg(level: LogLevel, msg: String) extends DSL[Unit]

  /** just helpers without any weird implicits */
  def debug(msg: String) = LogMsg(DebugLevel, msg)
  def info(msg: String) = LogMsg(InfoLevel, msg)
  def infoF(msg: String): Free[DSL, Unit] = Free.liftF(info(msg))
}

//////////////////////////////////////////////////////////////////////////
// DB DSL
object DB {

  // DB DSL
  type Entity = Map[String, String]

  sealed trait DBError
  case object NotFound extends DBError

  sealed trait DSL[A]
  case class FindById(id: String) extends DSL[Xor[DBError, Entity]]

}



//////////////////////////////////////////////////////////////////////////
// Http DSL
object Http {

  // Http DSL
  sealed trait  HttpVerb
  case object   Get extends HttpVerb
  case object   Post extends HttpVerb

  sealed trait  HttpStatus                              { val value: Int  }
  case object   Ok                  extends HttpStatus  { val value = 200 }
  case object   BadRequest          extends HttpStatus  { val value = 400 }
  case object   InternalServerError extends HttpStatus  { val value = 500 }

  type Params = Map[String, Seq[String]]
  type Headers = Map[String, Seq[String]]

  sealed trait HttpReq {
    val verb: HttpVerb
    val url: String
    val params: Params
    val headers: Headers
  }

  case class GetReq(
    url: String,
    params: Params = Map.empty[String, Seq[String]],
    headers: Headers = Map.empty[String, Seq[String]]
  ) extends HttpReq {
    val verb = Get
  }

  case class PostReq(
    url: String,
    params: Params = Map.empty[String, Seq[String]],
    headers: Headers = Map.empty[String, Seq[String]],
    body: String
  ) extends HttpReq {
    val verb = Post
  }

  case class HttpResp (
    status: HttpStatus,
    headers: Headers = Map.empty[String, Seq[String]],
    body: String = ""
  )

  sealed trait  RecvError
  case object   ClientDisconnected extends RecvError
  case object   Timeout extends RecvError

  sealed trait  SendStatus
  case object   Ack extends SendStatus
  case object   NAck extends SendStatus

  sealed trait  HttpInteract[A]
  case object   HttpReceive extends HttpInteract[Xor[RecvError, HttpReq]]
  case class    HttpRespond(data: HttpResp) extends HttpInteract[SendStatus]
  case class    Stop(error: Xor[RecvError, SendStatus]) extends HttpInteract[Xor[RecvError, SendStatus]]

  object HttpInteract {
    def receive() = HttpReceive
    def respond(data: HttpResp) = HttpRespond(data)
    def stop(err: Xor[RecvError, SendStatus]) = Stop(err)
  }

  sealed trait  HttpHandle[A]
  case class    HttpHandleResult(resp: HttpResp) extends HttpHandle[HttpResp]

  object HttpHandle {
    def result(resp: HttpResp) = HttpHandleResult(resp)
  }
}


class AppSpec extends FlatSpec with Matchers {

  /** weird this is not provided in cats apparently */
  implicit val fc = new cats.Comonad[Function0] {
    def extract[A](x: () => A): A = x()
    def coflatMap[A, B](fa: () => A)(f: (() => A) => B): () => B = () => f(fa)
    def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())
  }


  "ShapeApp" should "freek" in {

    object DBService {
      import DB._

      // APP DEFINITION
      // DSL.Make DSL in a higher-kinded coproduct
      // Log.DSL :@: DB.DSL :@: NilDSL builds (A => Log.DSL[A] :+: DB.DSL[A] :+: CNilK[A])
      // NilDSL corresponds to a higher-kinded CNil or no-effect combinator
      // without it, it's impossible to build to higher-kinded coproduct in a clea way
      type PRG = Log.DSL :|: DB.DSL :|: NilDSL
      val PRG = DSL.Make[PRG]

      /** the DSL.Make */
      def findById(id: String): Free[PRG.Cop, Xor[DBError, Entity]] =
        for {
          _    <- Log.debug("Searching for entity id:"+id).freek[PRG]
          res  <- FindById(id).freek[PRG]
          _    <- Log.debug("Search result:"+res).freek[PRG]
        } yield (res)
    }

    object HttpService {
      import Http._

      /** Combining DSL in a type alias */
      type PRG = Log.DSL :|: HttpInteract :|: HttpHandle :|: DBService.PRG
      val PRG = DSL.Make[PRG]

      // Handle action
      // :@@: DSL.Makes a F[_] with an existing higher-kinded coproduct
      def handle(req: HttpReq): Free[PRG.Cop, HttpResp] = req.url match {
        case "/foo" =>
          for {
            _     <-  Log.debug("/foo").freek[PRG]
            dbRes <-  DBService.findById("foo").expand[PRG]

            resp  <-  HttpHandle.result(
                        dbRes match {
                          case Xor.Left(err) => HttpResp(status = InternalServerError)
                          case Xor.Right(e)   => HttpResp(status = Ok, body = e.toString)
                        }
                      ).freek[PRG]
          } yield (resp)

        case _ => HttpHandle.result(HttpResp(status = InternalServerError)).freek[PRG]
      }

      // server DSL.Make
      // this is the worst case: recursive call so need to help scalac a lot
      // but in classic cases, it should be much more straighforward
      def serve() : Free[PRG.Cop, Xor[RecvError, SendStatus]] =
        for {
          recv  <-  HttpInteract.receive().freek[PRG]
          _     <-  Log.info("HttpReceived Request:"+recv).freek[PRG]
          res   <-  recv match {
                      case Xor.Left(err) => HttpInteract.stop(Xor.left(err)).freek[PRG]

                      case Xor.Right(req) =>
                        for {
                          resp  <-  handle(req)
                          _     <-  Log.info("Sending Response:"+resp).freek[PRG]
                          ack   <-  HttpInteract.respond(resp).freek[PRG]
                          res   <-  if(ack == Ack) serve()
                                    else HttpInteract.stop(Xor.right(ack)).freek[PRG]
                        } yield (res)
                    }
        } yield (res)

    }


    //////////////////////////////////////////////////////////////////////////
    // Interpreters as simple TransNat
    object Logger extends (Log.DSL ~> cats.Id) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          println(s"$lvl $msg")
      }
    }

    object DBManager extends (DB.DSL ~> cats.Id) {
      def apply[A](a: DB.DSL[A]) = a match {
        case DB.FindById(id) =>
          println(s"DB Finding $id")
          Xor.right(Map("id" -> id, "name" -> "toto"))
      }
    }

    object HttpHandler extends (Http.HttpHandle ~> cats.Id) {
      def apply[A](a: Http.HttpHandle[A]) = a match {
        case Http.HttpHandleResult(resp) =>
          println(s"Handling $resp")
          resp
      }
    }

    object HttpInteraction extends (Http.HttpInteract ~> cats.Id) {
      var i = 0
      def apply[A](a: Http.HttpInteract[A]) = a match {
        case Http.HttpReceive       =>
          if(i < 10000) {
            i+=1
            Xor.right(Http.GetReq("/foo"))
          } else {
            Xor.left(Http.ClientDisconnected)
          }

        case Http.HttpRespond(resp) => Http.Ack

        case Http.Stop(err) => err
      }
    }

    /** let's DSL.Make interpreters into a big interpreter
      * (F ~> R) :+: (G ~> R) => [t => F[t] :+: G[t] :+: CNilK[t]] ~> R
      */
    val interpreter = HttpInteraction :&: Logger :&: HttpHandler :&: DBManager

    /** as we use a recursive DSL.Make, we need to trampoline it in order to prevent stack overflow */
    object Trampolined extends (cats.Id ~> Trampoline) {
      def apply[A](a: cats.Id[A]) = Trampoline.done(a)
    }

    // execute final DSL.Make as a simple free with DSL.Maked interpreter composed with a trampoline
    HttpService.serve().interpret(interpreter andThen Trampolined).run
    println(HttpInteraction.i)

  }


  "freek" should "manage monad transformers" in {
    import cats.instances.future._
    import cats.data.OptionT
    import ExecutionContext.Implicits.global
    // import hk._

    sealed trait Foo[A]
    final case class Bar(s: String) extends Foo[Option[Int]]
    final case class Bar2(i: Int) extends Foo[Xor[String, Int]]
    final case object Bar3 extends Foo[Unit]

    type PRG = Foo :|: Log.DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    val prg = for {
      i     <- Bar("5").freek[PRG].liftT[Option].liftF[Xor[String, ?]]
      i     <- Bar2(i).freek[PRG].liftF[Option].liftT[Xor[String, ?]]
      _     <- Log.info("toto " + i).freek[PRG].liftF[Option].liftF[Xor[String, ?]]
      _     <- Log.infoF("").expand[PRG].liftF[Option].liftF[Xor[String, ?]]
      _     <- Bar3.freek[PRG].liftF[Option].liftF[Xor[String, ?]]
    } yield (())

    val logger2FutureSkip = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2FutureSkip = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Bar(s) => Future { Some(s.toInt) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i))
        case Bar3 => Future.successful(())
      }
    }

    val interpreters = foo2FutureSkip :&: logger2FutureSkip

    Await.result(prg.value.value.interpret(interpreters), 10.seconds)
  
  }

  "freek" should "manage monadic onions of result types" in {
    import cats.instances.future._
    import cats.instances.option._
    import cats.instances.list._
    import ExecutionContext.Implicits.global

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[List[Option[Int]]]
    final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
    final case object Foo3 extends Foo[Unit]
    final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[Option[String]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Option :&: Bulb

    type PRG = Foo :|: Log.DSL :|: PRG2
    val PRG = DSL.Make[PRG]

    val prg = for {
      i     <- Foo1("5").freek[PRG].onionT[O]
      i2    <- Foo2(i).freek[PRG].onionT[O]
      _     <- Log.info("toto " + i).freek[PRG].onionT[O]
      _     <- Foo3.freek[PRG].onionT[O]
      s     <- Bar1(i2.toString).freek[PRG].onionT[O]
      i3    <- Foo4(i2).freek[PRG].onionT[O]
    } yield (i3)

    val logger2Future = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2Future = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Foo1(s) => Future { List(Some(s.toInt)) } // if you put None here, it stops prg before Log
        case Foo2(i) => Future(Xor.right(i))
        case Foo3 => Future.successful(())
        case Foo4(i) => Future.successful(Xor.right(Some(i)))
      }
    }

    val bar2Future = new (Bar ~> Future) {
      def apply[A](a: Bar[A]) = a match {
        case Bar1(s) => Future { Some(s) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i.toString))
      }
    }

    val interpreters = foo2Future :&: logger2Future :&: bar2Future

    Await.result(prg.value.interpret(interpreters), 10.seconds)
  
  }

  "freek" should "manage monadic onions of result types manipulating Option[A] using Onion" in {
    import cats.instances.future._
    import cats.instances.option._
    import cats.instances.list._
    import ExecutionContext.Implicits.global

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[Option[Int]]
    final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
    final case object Foo3 extends Foo[Unit]
    final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[List[Option[String]]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Bulb

    type PRG = Foo :|: Log.DSL :|: PRG2
    val PRG = DSL.Make[PRG]

    val prg = for {
      iOpt  <-  Foo1("5").freek[PRG].onion[O]
      i2    <-  iOpt match {
                  case Some(i) => Foo2(i).freek[PRG].onionT[O]
                  case None => Foo2(0).freek[PRG].onionT[O]
                }
      _     <-  Log.info("toto " + i2).freek[PRG].onionT[O]
      _     <-  Foo3.freek[PRG].onionT[O]
      s     <-  Bar1(i2.toString).freek[PRG].onion[O]
      i3    <-  Foo4(i2).freek[PRG].onion[O]
    } yield (i3)

    val logger2Future = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2Future = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Foo1(s) => Future { Some(s.toInt) } // if you put None here, it stops prg before Log
        case Foo2(i) => Future(Xor.right(i))
        case Foo3 => Future.successful(())
        case Foo4(i) => Future.successful(Xor.right(Some(i)))
      }
    }

    val bar2Future = new (Bar ~> Future) {
      def apply[A](a: Bar[A]) = a match {
        case Bar1(s) => Future { List(Some(s)) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i.toString))
      }
    }

    val interpreters = foo2Future :&: logger2Future :&: bar2Future

    Await.result(prg.value.interpret(interpreters), 10.seconds)
  
  }

  "freek" should "manage monadic onions of result types 3" in {
    import cats.instances.future._
    import cats.instances.option._
    import cats.instances.list._
    import ExecutionContext.Implicits.global

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[Option[Int]]
    final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
    final case object Foo3 extends Foo[Unit]
    final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[List[Option[String]]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Option :&: Bulb

    type PRG = Foo :|: Log.DSL :|: PRG2
    val PRG = DSL.Make[PRG]

    val prg = for {
      iOpt  <-  Foo1("5").freek[PRG].onionT[O].peelRight
      i2    <-  iOpt match {
                  case Some(i) => Foo2(i).freek[PRG].onionT[O].peelRight
                  case None => Foo2(0).freek[PRG].onionT[O].peelRight
                }
      _     <-  Log.info("toto " + i2).freek[PRG].onionT[O].peelRight
      _     <-  Foo3.freek[PRG].onionT[O].peelRight
      s     <-  Bar1(i2.toString).freek[PRG].onionT[O].peelRight
      i3    <-  i2 match {
                  case Some(i) => Foo4(i).freek[PRG].onionT[O].peelRight
                  case None => Foo4(0).freek[PRG].onionT[O].peelRight
                }
    } yield (i3)

    val logger2Future = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2Future = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Foo1(s) => Future { Some(s.toInt) } // if you put None here, it stops prg before Log
        case Foo2(i) => Future(Xor.right(i))
        case Foo3 => Future.successful(())
        case Foo4(i) => Future.successful(Xor.right(Some(i)))
      }
    }

    val bar2Future = new (Bar ~> Future) {
      def apply[A](a: Bar[A]) = a match {
        case Bar1(s) => Future { List(Some(s)) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i.toString))
      }
    }

    val interpreters = foo2Future :&: logger2Future :&: bar2Future

    Await.result(prg.value.interpret(interpreters), 10.seconds)
  
  }

  "freek" should "manage monadic onions of result types with phantom types (upcasting)" in {
    import cats.instances.future._
    import cats.instances.option._
    import cats.instances.list._
    import ExecutionContext.Implicits.global


    sealed trait KVS[K, V, E]
    case class Get[K, V](key: K) extends KVS[K, V, V]
    case class Put[K, V](key: K, value: V) extends KVS[K, V, Unit]

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[List[Option[Int]]]
    final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
    final case object Foo3 extends Foo[Unit]
    final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[Option[String]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Option :&: Bulb

    type PRG = Foo :|: Log.DSL  :|: KVS[String, Int, ?] :|: PRG2
    val PRG = DSL.Make[PRG]

    val prg = for {
      i     <- Foo1("5").freek[PRG].onionT[O]
      i2    <- Foo2(i).freek[PRG].onionT[O]
      _     <- Put[String, Int](i.toString, i2).upcast[KVS[String, Int, Unit]].freek[PRG].onionT[O]
      _     <- Get[String, Int](i.toString).upcast[KVS[String, Int, Int]].freek[PRG].onionT[O]
      _     <- Log.info("toto " + i).freek[PRG].onionT[O]
      _     <- Foo3.freek[PRG].onionT[O]
      s     <- Bar1(i2.toString).freek[PRG].onionT[O]
      i3    <- Foo4(i2).freek[PRG].onionT[O]
    } yield (i3)

    val logger2Future = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2Future = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Foo1(s) => Future { List(Some(s.toInt)) } // if you put None here, it stops prg before Log
        case Foo2(i) => Future(Xor.right(i))
        case Foo3 => Future.successful(())
        case Foo4(i) => Future.successful(Xor.right(Some(i)))
      }
    }

    val bar2Future = new (Bar ~> Future) {
      def apply[A](a: Bar[A]) = a match {
        case Bar1(s) => Future { Some(s) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i.toString))
      }
    }

    val kvs2Future = new (KVS[String, Int, ?] ~> Future) {
      val map = scala.collection.mutable.Map[String, Int]()

      def apply[A](a: KVS[String, Int, A]) = a match {
        case get:Get[String, Int] => Future { map(get.key) }
        case put:Put[String, Int] => Future { map += (put.key -> put.value); () }
      }
    }

    val interpreters = foo2Future :&: logger2Future :&: bar2Future :&: kvs2Future

    Await.result(prg.value.interpret(interpreters), 10.seconds)
  
  }  

  "freek" should "manage monadic onions of result types wrap/peelRight" in {

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[Option[Int]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[List[Option[String]]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Option :&: Bulb

    type PRG = Foo :|: Log.DSL :|: PRG2
    val PRG = DSL.Make[PRG]

    val f: OnionT[Free, PRG.Cop, List :&: Xor[String, ?] :&: Bulb, Option[Int]] =
      Foo1("5")
      .freek[PRG]
      .onionT[Xor[String, ?] :&: Option :&: Bulb]
      .wrap[List]
      .peelRight
  
  }

  "freek" should "manage monadic onions with freeko" in {
    import cats.instances.future._
    import cats.instances.option._
    import cats.instances.list._
    import ExecutionContext.Implicits.global

    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[List[Option[Int]]]
    final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
    final case object Foo3 extends Foo[Unit]
    final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[Option[String]]
    final case class Bar2(i: Int) extends Bar[Xor[String, String]]

    type PRG2 = Bar :|: Log.DSL :|: NilDSL

    type O = List :&: Xor[String, ?] :&: Option :&: Bulb

    type PRG = Foo :|: Log.DSL  :|: PRG2
    val PRG = DSL.Make[PRG]

    val prg: OnionT[Free, PRG.Cop, O, Int] = for {
      i     <- Foo1("5").freeko[PRG, O]
      i2    <- Foo2(i).freeko[PRG, O]
      _     <- Log.info("toto " + i).freeko[PRG, O]
      _     <- Foo3.freeko[PRG, O]
      s     <- Bar1(i2.toString).freeko[PRG, O]
      i3    <- Foo4(i2).freeko[PRG, O]
    } yield (i3)

    val logger2Future = new (Log.DSL ~> Future) {
      def apply[A](a: Log.DSL[A]) = a match {
        case Log.LogMsg(lvl, msg) =>
          Future.successful(println(s"$lvl $msg"))
      }
    }

    val foo2Future = new (Foo ~> Future) {
      def apply[A](a: Foo[A]) = a match {
        case Foo1(s) => Future { List(Some(s.toInt)) } // if you put None here, it stops prg before Log
        case Foo2(i) => Future(Xor.right(i))
        case Foo3 => Future.successful(())
        case Foo4(i) => Future.successful(Xor.right(Some(i)))
      }
    }

    val bar2Future = new (Bar ~> Future) {
      def apply[A](a: Bar[A]) = a match {
        case Bar1(s) => Future { Some(s) } // if you put None here, it stops prg before Log
        case Bar2(i) => Future(Xor.right(i.toString))
      }
    }

    val interpreters = foo2Future :&: logger2Future :&: bar2Future

    Await.result(prg.value.interpret(interpreters), 10.seconds)
  
  }

  "freek" should "allow declaring local DSL.Makes" in {
    
    trait RepositoryLayer {
      sealed trait Account

      sealed trait RepoF[A]

      sealed trait Repo[A]
      case class Query(no: String) extends Repo[Xor[String, Account]]
      case class Store(account: Account) extends Repo[Xor[String, Account]]
      case class Delete(no: String) extends Repo[Xor[String, Unit]]

      object Repo {
        type PRG = Repo :|: NilDSL
        type O = Xor[String, ?] :&: Bulb
      }

      def query(no: String) = Query(no)
      def store(account: Account) = Store(account)
      def delete(no: String) = Delete(no)

      // How do I write this function here ?     
      def update(no: String, f: Account => Account) = for {
        a <-  Query(no).freeko[Repo.PRG, Repo.O]
        _ <-  Store(f(a)).freeko[Repo.PRG, Repo.O]
      } yield (())
    }

    trait FooLayer extends RepositoryLayer {
      sealed trait Foo[A]
      final case class Foo1(s: String) extends Foo[List[Option[Int]]]
      final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
      final case object Foo3 extends Foo[Unit]
      final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

      object Foo {
        type PRG = Foo :|: Log.DSL :|: Repo.PRG
      }
    }

    trait BarLayer extends RepositoryLayer {

      sealed trait Bar[A]
      final case class Bar1(s: String) extends Bar[Option[String]]
      final case class Bar2(i: Int) extends Bar[Xor[String, String]]

      object Bar {
        type PRG = Bar :|: Log.DSL :|: Repo.PRG
      }

    }

    object Prg
      extends FooLayer
      with BarLayer {

      type O = List :&: Xor[String, ?] :&: Option :&: Bulb

      type PRG = Log.DSL :|: Bar.PRG :||: Foo.PRG
      val PRG = DSL.Make[PRG]

      val prg: OnionT[Free, PRG.Cop, O, Int] = for {
        i     <- Foo1("5").freeko[PRG, O]
        i2    <- Foo2(i).freeko[PRG, O]
        _     <- Log.info("toto " + i).freeko[PRG, O]
        _     <- Foo3.freeko[PRG, O]
        s     <- Bar1(i2.toString).freeko[PRG, O]
        i3    <- Foo4(i2).freeko[PRG, O]
        _     <- update(i.toString, identity).freeko[PRG, O]
      } yield (i)

      val logger2Future = new (Log.DSL ~> Future) {
        def apply[A](a: Log.DSL[A]) = a match {
          case Log.LogMsg(lvl, msg) =>
            Future.successful(println(s"$lvl $msg"))
        }
      }

      val foo2Future = new (Foo ~> Future) {
        def apply[A](a: Foo[A]) = a match {
          case Foo1(s) => Future { println(s); List(Some(s.toInt)) } // if you put None here, it stops prg before Log
          case Foo2(i) => Future(Xor.right(i))
          case Foo3 => Future.successful(())
          case Foo4(i) => Future.successful(Xor.right(Some(i)))
        }
      }

      val bar2Future = new (Bar ~> Future) {
        def apply[A](a: Bar[A]) = a match {
          case Bar1(s) => Future { Some(s) } // if you put None here, it stops prg before Log
          case Bar2(i) => Future(Xor.right(i.toString))
        }
      }

      val repo2Future = new (Repo ~> Future) {
        def apply[A](a: Repo[A]) = a match {
          case Query(s) => Future { Xor.right(new Account {}) }
          case Store(acc) => Future { Xor.right(new Account {}) }
          case Delete(no) => Future { Xor.right(()) }
        }
      }

      val fooInterpreters = foo2Future :&: logger2Future :&: repo2Future
      val barInterpreters = bar2Future :&: logger2Future :&: repo2Future

      val interpreters = foo2Future :&: logger2Future :&: bar2Future :&: repo2Future
      val interpreters2 = logger2Future :&: fooInterpreters :&&: barInterpreters
    }
    val r = Await.result(Prg.prg.value.interpret(Prg.interpreters), 10.seconds)
    println("result:"+r)
    val r2 = Await.result(Prg.prg.value.interpret(Prg.interpreters2), 10.seconds)
    println("result:"+r2)
  }


  "freek" should "special cases" in {
    sealed trait Foo[A]
    final case class Foo1(s: String) extends Foo[List[String]]

    sealed trait Bar[A]
    final case class Bar1(s: String) extends Bar[Option[String]]

    sealed trait KVS[K, V, E]
    case class Get[K, V](key: K) extends KVS[K, V, Option[V]]
    case class Put[K, V](key: K, value: V) extends KVS[K, V, Unit]

    type KVSA[A] = KVS[String, Int, A]
    type PRG = KVSA :|: KVS[Float, Double, ?] :|: Foo :|: Bar :|: NilDSL
    val PRG = DSL.Make[PRG]
    type O = Option :&: Bulb

    val f1 = for {
      _ <- Bar1("bar1").freek[PRG].onionT[O]
      _ <- Foo1("foo1").freek[PRG].onion[O]
    } yield (())

    val f2: Free[PRG.Cop, Option[Int]] = for {
      i <- Get[String, Int]("toto").upcast[KVSA[Option[Int]]].freek[PRG]
    } yield (i)

    val f3: Free[PRG.Cop, Option[Int]] = Get[String, Int]("toto").upcast[KVSA[Option[Int]]].freek[PRG]
  }

  "freek" should "special cases 2" in {
    sealed trait Foo1[A]
    final case class Bar1(s: Int) extends Foo1[List[Int]]

    sealed trait Foo2[A]
    final case class Bar21(s: Int) extends Foo2[Option[Int]]
    final case class Bar22(s: Int) extends Foo2[List[Option[Int]]]

    sealed trait Foo3[A]
    final case class Bar31(s: Long) extends Foo3[Xor[String, Long]]
    final case class Bar32(s: Float) extends Foo3[Xor[String, List[Float]]]
    final case class Bar33(s: Double) extends Foo3[Xor[String, Option[Boolean]]]
    
    type PRG = Foo1 :|: Foo2 :|: Foo3 :|: NilDSL
    val PRG = DSL.Make[PRG]
    type O = Xor[String, ?] :&: List :&: Option :&: Bulb

    val f1: Free[PRG.Cop, Xor[String, List[Option[Unit]]]] = (for {
      i <- Bar1(3).freek[PRG].onionT[O]
      i <- Bar21(i).freek[PRG].onionT[O]
      i <- Bar22(i).freek[PRG].onionT[O]
      l <- Bar31(i.toLong).freek[PRG].onionT[O]
      l <- Bar32(l.toFloat).freek[PRG].onionT[O]
      _ <- Bar33(l.toDouble).freek[PRG].onionT[O]
    } yield (())).value

  }

  "freek" should "special cases 3" in {
    sealed trait Foo1[A]
    final case class Bar1(s: Int) extends Foo1[List[Int]]

    sealed trait Foo2[A]
    final case class Bar21(s: Int) extends Foo2[Option[Int]]
    final case class Bar22(s: Int) extends Foo2[List[Option[Int]]]

    sealed trait Foo3[A]
    final case class Bar31(s: Int) extends Foo3[Xor[String, Long]]
    final case class Bar32(s: Float) extends Foo3[Xor[String, List[Float]]]
    final case class Bar33(s: Double) extends Foo3[Xor[String, Option[Boolean]]]
    final case class Bar34(s: Double) extends Foo3[Xor[String, List[Option[Boolean]]]]
    
    type PRG = Foo1 :|: Foo2 :|: Foo3 :|: NilDSL
    val PRG = DSL.Make[PRG]
    type O = Xor[String, ?] :&: List :&: Option :&: Bulb

    // ugly head & get :D
    val f1: Free[PRG.Cop, Xor[String, String]] = (for {
      i   <- Bar1(3).freek[PRG].onionT2[O]
      i   <- Bar21(i.head.get).freek[PRG].onionT2[O]
      i   <- Bar22(i.head.get).freek[PRG].onionT2[O]
      i   <- Bar31(i.head.get).freek[PRG].onionT2[O]
      i   <- Bar32(i.head.get.toFloat).freek[PRG].onionT2[O]
      i   <- Bar33(i.head.get.toDouble).freek[PRG].onionT2[O]
    } yield (i.toString)).value

  }

  "freek" should "special cases 4" in {
    sealed trait Foo1[A]
    final case class Bar11(s: Int) extends Foo1[Xor[String, List[Int]]]
    final case class Bar12(s: List[Int]) extends Foo1[Xor[String, Option[Int]]]

    sealed trait Foo2[A]
    final case class Bar21(s: Int) extends Foo1[Xor[Long, Option[List[Int]]]]
    final case class Bar22(s: List[Int]) extends Foo1[Xor[Long, Option[Int]]]

    type PRG = Foo1 :|: Foo2 :|: NilDSL
    val PRG = DSL.Make[PRG]
    type O = Xor[String, ?] :&: Xor[Long, ?] :&: Option :&: Bulb

    val f1: OnionT[Free, PRG.Cop, O, Unit] = for {
      l1 <- Bar11(5).freek[PRG].onionX1[O]
      _  <- Bar12(l1).freek[PRG].onionT[O]
      l2 <- Bar21(6).freek[PRG].onionX2[O]
      _  <- Bar22(l2).freek[PRG].onionT[O]
    } yield (())

  }

  "freek" should "extend DSL" in {
    object Program {
      sealed trait Foo1[A]
      final case class Bar11(s: Int) extends Foo1[String]

      sealed trait Foo2[A]
      final case class Bar21(s: String) extends Foo2[Int]

      type PRG = Foo1 :|: Foo2 :|: NilDSL
      val PRG = DSL.Make[PRG]

      val program = for {
        s <- Bar11(5).freek[PRG]
        i <- Bar21(s).freek[PRG]
      } yield (i)
    }

    object OtherProgram {
      import Program._
      
      sealed trait Foo3[A]
      case class Bar31[A](bar11: Foo1[A]) extends Foo3[A]
      case class Bar32(i: Int) extends Foo3[String]

      type PRG = Foo3 :|: Foo2 :|: NilDSL
      val PRG = DSL.Make[PRG]

      val copknat = CopKNat[Program.PRG.Cop].replace(
        new (Foo1 ~> Foo3) {
          def apply[A](foo1: Foo1[A]): Foo3[A] = Bar31(foo1)
        }
      )

      val program = for {
        i <- Program.program.compile(copknat)
        s <- Bar32(i).freek[PRG]
      } yield (s)

    }

    import Program._
    import OtherProgram._

    val foo1Future = new (Foo1 ~> Future) {
      def apply[A](a: Foo1[A]) = a match {
        case Bar11(i) => Future { i.toString }
      }
    }

    val foo2Future = new (Foo2 ~> Future) {
      def apply[A](a: Foo2[A]) = a match {
        case Bar21(s) => Future { s.toInt }
      }
    }

    def foo3Future(foo1Nat: Foo1 ~> Future) = new (Foo3 ~> Future) {
      def apply[A](a: Foo3[A]) = a match {
        case Bar31(foo1) => foo1Nat(foo1)
        case Bar32(i) => Future { i.toString }
      }
    }

    val interpreter = foo2Future :&: foo3Future(foo1Future)

    val fut = OtherProgram.program.interpret(interpreter)

    ()
  } 

  "freek" should "transpile" in {
    object Program {
      sealed trait Foo1[A]
      final case class Bar11(s: Int) extends Foo1[Int]

      sealed trait Foo2[A]
      final case class Bar21(s: String) extends Foo2[String]

      type PRG = Foo1 :|: Foo2 :|: NilDSL
      val PRG = DSL.Make[PRG]

      val program = for {
        _ <- Bar11(5).freek[PRG]
        _ <- Bar21("1.234").freek[PRG]
      } yield (())
    }

    object OtherProgram {

      sealed trait Foo3[A]
      final case class Bar31(s: String) extends Foo3[Float]

      sealed trait Foo4[A]
      final case class Bar41(s: Float) extends Foo4[String]

      type PRG = Foo3 :|: Foo4 :|: NilDSL
      val PRG = DSL.Make[PRG]

      // this is our transpiler transforming a Foo2 into another free program
      val transpiler = new (Program.Foo2 ~> Free[PRG.Cop, ?]) {

        def apply[A](f: Program.Foo2[A]): Free[PRG.Cop, A] = f match {
          case Program.Bar21(s) =>
            for {
             f <- Bar31(s).freek[PRG]
             s <- Bar41(f).freek[PRG]
            } yield (s)
        }
      }
    }

    import Program._
    import OtherProgram._

    // 1/ CopKNat[Program.PRG.Cop] creates a Program.PRG.Cop ~> Program.PRG.Cop
    // 2/ .replace creates a natural trans that replaces Program.Foo2 in Program.PRG.Cop by Free[OtherProgram.PRG.Cop, ?] using transpiler
    // 3/ The result is a terrible natural transformation (don't try to write that type, it's too ugly, let's scalac do it) :
    //    (Foo1 :|: Foo2 :|: NilDSL) ~> (Foo1 :|: Free[OtherProgram.PRG.Cop, ?] :|: NilDSL)
    val transpileNat = CopKNat[Program.PRG.Cop].replace(OtherProgram.transpiler)

    // Transpile does 2 operations:
    // 1/ Replaces Foo2 in Program.PRG.Cop by Free[OtherProgram.PRG.Cop, A]
    //    -> OtherProgram.transpiler natural transformation converts Foo2 into the free program Free[OtherProgram.PRG.Cop, A]
    //    -> New PRG.Cop is then Foo1 :|: Free[OtherProgram.PRG.Cop, ?] :|: NilDSL
    //
    // 2/ Flattens Free[(Foo1 :|: Free[(Foo3 :|: Foo4 :|: NilDSL)#Cop, ?] :|: NilDSL)#Cop, A] into
    //    Free[(Foo1 :|: Foo3 :|: Foo4 :|: NilDSL)#Cop, A]
    val free = Program.program.transpile(transpileNat)
    // Same as
    // val free2 = Program.f.compile(transpileNat).flatten

    // Write our interpreters for new program (Foo1, Foo3, Foo4)
    val foo1Future = new (Foo1 ~> Future) {
      def apply[A](a: Foo1[A]) = a match {
        case Bar11(i) => Future { i }
      }
    }

    val foo3Future = new (Foo3 ~> Future) {
      def apply[A](a: Foo3[A]) = a match {
        case Bar31(s) => Future { s.toFloat }
      }
    }

    val foo4Future = new (Foo4 ~> Future) {
      def apply[A](a: Foo4[A]) = a match {
        case Bar41(s) => Future { s.toString }
      }
    }

    val r = Await.result(free.interpret(foo1Future :&: foo3Future :&: foo4Future), 2.seconds)
    println("r:"+r)
  }

}

