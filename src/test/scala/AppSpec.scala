/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  */
import org.scalatest._

import cats.free.{Free, Trampoline}
import cats.data.Xor
import cats.{~>, Id}

import scala.concurrent._

// import cats.derived._, functor._, legacy._
import cats.Functor

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


  // "foldMap" should "be stack safe" in {
  //   trait FTestApi[A]
  //   case class TB(i: Int) extends FTestApi[Int]

  //   type FTest[A] = Free[FTestApi, A]

  //   def tb(i: Int): FTest[Int] = Free.liftF(TB(i))

  //   def a(i: Int): FTest[Int] = for {
  //     j <- tb(i)
  //     z <- if (j<10000000) a(j) else Free.pure[FTestApi, Int](j)
  //   } yield z

  //   def runner: FTestApi ~> Trampoline = new (FTestApi ~> Trampoline) {
  //     def apply[A](fa: FTestApi[A]): Trampoline[A] = fa match {
  //       case TB(i) => Trampoline.done(i+1)
  //     }
  //   }

  //   assert(10000000 == a(0).foldMap(runner).run)
  // }

  "ShapeApp" should "freek" in {

    /** Declare programs */
    object DBService {
      import DB._

      // APP DEFINITION
      // combine DSL in a higher-kinded coproduct
      // (Log.DSL :@: DB.DSL :@: FXNil)#Cop[A] builds (A => Log.DSL[A] :+: DB.DSL[A] :+: CNilK[A])      
      // FXNil corresponds to a higher-kinded CNil or no-effect combinator
      // without it, it's impossible to build to higher-kinded coproduct in a clea way
      type PRG[A] = (Log.DSL :@: DB.DSL :@: FXNil)#Cop[A]

      /** the program */
      def findById(id: String): Freek[PRG, Xor[DBError, Entity]] = 
        for {
          _    <- Log.debug("Searching for entity id:"+id).freek
          res  <- FindById(id).freek
          // _    <- Log.debug("Search result:"+res).freek
        } yield (res)
    }

    object HttpService {
      import Http._

      // Handle action
      // :@@: combines a F[_] with an existing higher-kinded coproduct 
      def handle(req: HttpReq): Freek[(HttpHandle :@@: DBService.PRG)#Cop, HttpResp] = req.url match {
        case "/foo" =>
          for {
            dbRes <-  DBService.findById("foo")

            resp  <-  HttpHandle.result(
                        dbRes match {
                          case Xor.Left(err) => HttpResp(status = InternalServerError)
                          case Xor.Right(e)   => HttpResp(status = Ok, body = e.toString)
                        }
                      ).freek
          } yield (resp)

        case _ => HttpHandle.result(HttpResp(status = InternalServerError)).freek
      }

      /** Combining DSL in a type alias */
      type PRG[A] = (HttpInteract :@: HttpHandle :@@: DBService.PRG)#Cop[A]

      // server program
      // this is the worst case: recursive call so need to help scalac a lot
      // but in classic cases, it should be much more straighforward
      def serve(): Freek[PRG, Xor[RecvError, SendStatus]] =
        for {
          recv  <-  HttpInteract.receive().freek
          _     <-  Log.info("HttpReceived Request:"+recv).freek
          res   <-  (recv match {
                      case Xor.Left(err) => HttpInteract.stop(Xor.left(err)).freek

                      case Xor.Right(req) => 
                        for {
                          resp  <-  handle(req)
                          _     <-  Log.info("Sending Response:"+resp).freek
                          ack   <-  HttpInteract.respond(resp).freek
                          res   <-  if(ack == Ack) serve()
                                    else HttpInteract.stop(Xor.right(ack)).freek: Freek[PRG, Xor[RecvError, SendStatus]] //.map(_ => ()):Freek[App, Unit]
                        } yield (res)
                    }): Freek[PRG, Xor[RecvError, SendStatus]]
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
          if(i < 1000) {
            i+=1
            Xor.right(Http.GetReq("/foo"))
          } else {
            Xor.left(Http.ClientDisconnected)
          }

        case Http.HttpRespond(resp) => Http.Ack

        case Http.Stop(err) => err
      }
    }

    /** let's combine interpreters into a big interpreter
      * (F ~> R) >>: (G ~> R) => [t => F[t] :+: G[t] :+: CNilK[t]] ~> R
      */
    val interpreter: Interpreter[HttpService.PRG, cats.Id] = HttpInteraction >>: HttpHandler >>: Logger >>: DBManager

    /** as we use a recursive program, we need to trampoline it in order to prevent stack overflow */
    object Trampolined extends (cats.Id ~> Trampoline) {
      def apply[A](a: cats.Id[A]) = Trampoline.done(a)
    }

    // execute final program as a simple free with combined interpreter composed with a trampoline
    try {
      HttpService.serve().free.foldMap(Trampolined compose interpreter.nat).run
    } catch {
      case e:Throwable => e.printStackTrace()
    }
  }

}



















    // trait F[A]
    // trait G[A]
    // trait I[A]
    // type H[A] = ConsK[G, ConsK[F, CNilK, ?], A]
    // ContainsHK[H, G]
    // ContainsHK[H, F]
    // implicitly[ContainsHK.Aux[H, G, ConsK[F, CNilK, ?]]]
    // implicitly[ContainsHK.Aux[H, F, ConsK[G, CNilK, ?]]]
    // ContainsHK[ConsK[F, CNilK, ?], F]

    // implicitly[MergeOneRightHK.Aux[ConsK[F, CNilK, ?], G, ConsK[F, ConsK[G, CNilK, ?], ?]]]
    // implicitly[MergeOneRightHK.Aux[ConsK[G, ConsK[F, CNilK, ?], ?], F, ConsK[G, ConsK[F, CNilK, ?], ?]]]
    // implicitly[MergeOneRightHK.Aux[ConsK[G, ConsK[F, CNilK, ?], ?], I, ConsK[G, ConsK[F, ConsK[I, CNilK, ?], ?], ?]]]

    // implicitly[MergeCopHK.Aux[ConsK[F, CNilK, ?], ConsK[G, CNilK, ?], ConsK[F, ConsK[G, CNilK, ?], ?]]]
    // implicitly[MergeCopHK.Aux[ConsK[F, ConsK[G, CNilK, ?], ?], ConsK[I, CNilK, ?], ConsK[F, ConsK[G, ConsK[I, CNilK, ?], ?], ?]]]
    // implicitly[MergeCopHK.Aux[ConsK[F, ConsK[G, CNilK, ?], ?], ConsK[F, CNilK, ?], ConsK[F, ConsK[G, CNilK, ?], ?]]]
    // implicitly[MergeCopHK.Aux[ConsK[F, ConsK[G, CNilK, ?], ?], ConsK[G, CNilK, ?], ConsK[F, ConsK[G, CNilK, ?], ?]]]




