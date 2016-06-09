## Freek, a freaky simple Free to combine your DSL seamlessly

> Pere Villega wrote a really cool introduction to Freek before I could even move a finger and it's a really good one :D 
>
> http://perevillega.com/freek-and-free-monads
>
> Thanks a lot Pere


## Current Version

### v0.3.0:

- replaced Freenion by OnionT which generalized the Onion embedding to any structure of type `TC[_[_], _]`
- provided new `interpret` function solving order/redundancy issues with DSL vs interpreters
- renamed Interpreter combining operator `:|:` into `:&:` because combining interpreters is about a sum between them, not a product of them


## Motivations

> At [ProjectSeptember](http://www.projectseptember.com), we love typesafe & functional programming.

We also like the concept of `Free Monad` which decouples completely the description of your program from its execution.

Free has a cost in terms of performance & short-term structures allocation but in our domain, IO is much more the bottleneck than these aspects so we can use those concepts without remorse.

We also believe current implementations can be improved progressively using theoretical and even more brutal tools like compiler plugin/optimizations. So we want to push those concepts further and further.

In Cats & Scalaz, `Free[F[_], A]` representation has already been optimized into a right associated structure and now embeds Coyoneda trick removing the dependency on Functor. Now we can use any effect/DSL `F[_]` in a `Free[F, A]`.

Free is often associated to effects management and in this context, there are also interesting newer approaches in Scala world:

- [Emm](https://github.com/djspiewak/emm) : A very clever idea to represent stack of effects and a nice implementation (from which a few ideas have been stolen for `freek`). It's interesting because it can use existing structures but it has many implicits meaning it has a cost at compile-time and a few questioning about its pure Monadic nature (TBD)...

- [Scala Eff](http://atnos-org.github.io/eff-cats/): a more theoretical and deeply interesting implementation based on [Freer Monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)'s paper by Oleg Kiselyov & friends. It's the next-gen of effects management but it also requires more _aware developers_ certainly... Next step of evangelization ;)

- [Idris Eff port](https://github.com/mandubian/scalaeff): this is my personal toy... In Idris, it's (almost) nice, in Scala, it's almost a monster and more an experiment showing it could work. But it's the _next-gen + 1/2/3/4_ IMHO so let's be patient and make it grow...

> But for now, `Free` concept starts to enter in mind of people so we want to use it as is (with a few enhancements).

Here is what you want to do in general with Free:

### Building DSL

A DSL is a Domain Specific Language corresponding to the operation allowed by your business domain and describing it but not supposing how it will be executed.

```scala

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

  /** just helpers */
  def debug(msg: String) = LogMsg(DebugLevel, msg)
  def info(msg: String) = LogMsg(InfoLevel, msg)
  // ...
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
  // ...
}

```

### Building simple programs using DSL

Based on you DSL, you can build description of your program as sequence of operations/computations.
Each computation will return a value when finished triggering the next step in the program. `Xor[DBError, Entity]` for `FindById` for example.

Computations can have side-effects and in FP approaches, we tend to represent those with Monads and sequence monads into a sequence of computations using for-comprehensions.

For example:

```scala
object DBService {

  // Pseudo code
  def findById(id: String): Free[F, A] = 
    for {
      _    <- Log.debug("Searching for entity id:"+id)
      res  <- FindById(id)
      _    <- Log.debug("Search result:"+res)
    } yield (res)
}
```

#### What is this `F[_]` here?

Logically, as you program uses Logs & DB operations, it should be a sort of combination of `Log.DSL[_]` and `DB.DSL[_]`.?

We could think about a _(Shapeless)_ Coproduct (A or B or C or ...)

```
// DB.DSL or Log.DSL
t => DB.DSL[t] :+: Log.DSL[t] :+: CNil
```

### The program again

```scala
// Pseudo scala code
type PRG[A] = DB.DSL[A] :+: Log.DSL[t] :+: CNil

def findById(id: String): Free[PRG, Xor[DBError, Entity]] =
  for {
    _    <- Log.debug("Searching for entity id:"+id)
    res  <- FindById(id)
    _    <- Log.debug("Search result:"+res)
  } yield (res)
```

### Executing the program

First, we need interpreters transforming every DSL combined in the `PRG` into an effectful computation. We can represent an interpreter by a natural transformation converting `DSL[A]` into a `Effectful[A]` where `Effectful` correspond the real execution performing eventual side-effects.

```scala
//////////////////////////////////////////////////////////////////////////
// Interpreters as simple TransNat
object Logger extends (Log.DSL ~> Future) {
  def apply[A](a: Log.DSL[A]) = a match {
    case Log.LogMsg(lvl, msg) =>
      Future(println(s"$lvl $msg"))
  }
}

object DBManager extends (DB.DSL ~> Future) {
  def apply[A](a: DB.DSL[A]) = a match {
    case DB.FindById(id) =>
      Future {
        println(s"DB Finding $id")
        Xor.right(Map("id" -> id, "name" -> "toto"))
      }
  }
}
```

Then we want to execute our `findById` program using those interpreters.
Free monads come equipped with an operation called `foldMap`:

```scala
// Pseudo scala code
val interpreter: F ~> Future = DBManager combine Logger
val f: Future[Xor[DBError, Entity]] = findById(XXX).foldMap(interpreter)
```

#### What is `F[_]` now?

Our `type PRG[A] = DB.DSL[A] :+: Log.DSL[t] :+: CNil` naturally!

#### What is `combine`?

It is the operation:

```scala
(DB.DSL ~> Id combine Log.DSL ~> Id) => (DB.DSL :+: Log.DSL :+: CNil) ~> Id
```

> **Nice isn't it?**

> **But doesn't work ouf of the box**

> - Shapeless Coproduct isn't very good for Coproduct of higher-kinded structures `F[_]` (or any other Coproducts I know).

> `t => F[t] :+: (t => G[t] :+: CNil)` IS NOT `t => F[t] :+: G[t] :+: CNil`

> - Combining interpreters like that doesn't work

> - As you manipulate higher-kinded structures, you quickly hit the sadly famous `SI2712` issue.


## Freek itself: a freaky simple Free to combine your DSL seamlessly

Freek is not so much, just a few helpers to make manipulating Free & DSL just straightforward.

Here are the helpers brought by Freek:

### Combine your DSL(s) with operator `:|:` (OR)

This is a specialized implementation of Shapeless Coproduct for higher-kinded structures allowing to combine multiple DSL in one single.

Imagine you have the following DSL

```
sealed trait Log[A]
case class LogMsg(level: LogLevel, msg: String) extends DSL[Unit]
object Log {
  def debug(msg: String) = LogMsg(DebugLevel, msg)
  def info(msg: String) = LogMsg(InfoLevel, msg)
}

sealed trait KVS[A]
object KVS {
  final case class Get(key: String) extends DSL[String]
  final case class Put(key: String, value: String) extends DSL[Unit]
}

sealed trait File[A]
object File {
  final case class GetFile(name: String) extends DSL[File]
  final case class DeleteFile(name: String) extends DSL[Unit]
}
```

You want to build a program that manipulate the 4 DSL together meaning you program will use Log `or` DB `or` Foo `or` Bar DSL (Sum/Coproduct of DSL).

In Freek, you would combine them like that:

```
type PRG[A] = (Log :|: KVS :|: File :|: FXNil)#Cop[A]
```

- `:|:` is a symbol but in Scala, there is no more elegant way to mix types...
- `FXNil` is required to end this Coproduct
- `#Cop[A]` builds the real hidden Coproduct type which is the ugly `CoproductK` that you don't really want to see in general.


### `.freek[PRG]` to lift all DSL operations to `PRG` in for-comprehension

Ok, now you're going to write a program with those DSL lifted into Free monads `Free[DSL[_], A]` using a classic for-comprehension.

In a for-comprehension, each line should have the same type to compile so you need to lift all `Free[DSL[_], A]` to the common Free type `Free[PRG, A]`.

The conversion `DSL[A] => Free[DSL, A] => Free[PRG, A]` can be done in a trivial way using `.freek[PRG]`.

```scala
type PRG[A] = (Log :|: KVS :|: File :|: FXNil)#Cop[A]

def program(id: String) = 
  for {
    _     <- Log.debug(s"Searching for value id: $id").freek[PRG]
    name  <- KVS.Get(id).freek[PRG]
    file  <- File.Get(name).freek[PRG]
    _     <- Log.debug(s"Found file:$file").freek[PRG]
  } yield (file)
```

- Every line must be lifter to `PRG`
- Some people will think about a implicit conversion to avoid having to write `freek[PRG]` but believe my own experience, inference in for-comprehension isn't perfect in Scala and as soon as you manipulate more complex programs, implicit conversion makes inference break with terrible errors.


### Combine interpreters using operator `:&:` (AND)

`program` just describe your sequence of operations but it doesn't execute it: it's just a data representation, a description of your computation.

Now you need to _interpret_ this description into an effectful execution and this is done with `interpreters` in Freek.
`Interpreters` in Freek are nothing else than NaturalTransformation/FunctionK `F[_] ~> G[_]` with some helpers to manipulate multipl combined DSL(s) defined above.

Here we suppose, we use an async execution context based on Scala `Future` not to be too fancy.

1. Define your interpreters per DSL

```
val LogInterpreter = new (Log ~> Future) {
  def apply[A](a: Log[A]) = a match {
    case Log.LogMsg(lvl, msg) =>
      Future(println(s"$lvl $msg"))
  }
}

val FileInterpreter = new (File ~> Future) {
  def apply[A](a: DB[A]) = a match {
    case File.Get(name) =>
      Future {
        FileManager.get(name)
      }

    case File.Delete(name) =>
      Future {
        FileManager.delete(name)
      }
  }
}

val KVSInterpreter = new (KVS ~> Future) {
  val storage = ...

  def apply[A](a: DB[A]) = a match {
    case KVS.Get(id) =>
      Future {
        storage.get(id)
      }
    case KVS.Put(id, value) =>
      Future {
        storage.put(id, value)
        ()
      }
  }
}

```

2. Combine interpreters into a big interpreter with `:&:`

Being able to execute your `program` means you are able to interpret DSL `Log` and `KVS` and `File`.
So you need an interpreter which is the sum of `KVSInterpreter` and `LogInterpreter` and `FileInterpreter`


In Freek, here is how you define that:

```
val interpreter = KVSInterpreter :&: LogInterpreter :&: FileInterpreter
```

- Remark that there is no equivalent to `FXNil` at the end
- `interpreter` is of type `Interpreter` which is just a wrapper around a `C ~> R`  where `C[_] <: CoproductK[_]`. If you want to access the underlying `PRG ~> Future`, just call `interpreter.nat`.

### Execute your program using `interpret`

`program` is just a `Free[PRG, A]` so you could use simply `foldMap` with your `interpreter.nat`.

But Freek provides a smarter function called `interpret` that makes the order of DSL vs interpreters not relevant.

```
val fut = program.interpret(interpreter) // this returns a Future[Unit]
```

- 


### SI2712 patch

SI2712 recent patch released by @milessabin has changed a lot the way we can build type-intensive libraries because we aren't limited by this terrible issue.

> We need that [PR](https://github.com/scala/scala/pull/5102#issuecomment-219868111) merged in Scala 2.12, it's really important ;)

In Freek, it is used by default as it allows writing code without ugly `.asInstanceOf` to help poor Scalac.

Thanks @milessabin again!

## Freek, finally all together

```

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

  /** just helpers */
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
// Program
object DBService {
  import DB._

  type PRG[A] = (Log.DSL :|: DB.DSL :|: FXNil)#Cop[A]

  /** the program */
  def findById(id: String): Free[PRG, Xor[DBError, Entity]] = 
    for {
      _    <- Log.debug("Searching for entity id:"+id).freek[PRG]
      res  <- FindById(id).freek[PRG]
      _    <- Log.debug("Search result:"+res).freek[PRG]
    } yield (res)
}

//////////////////////////////////////////////////////////////////////////
// Interpreters
object Logger extends (Log.DSL ~> Id) {
  def apply[A](a: Log.DSL[A]) = a match {
    case Log.LogMsg(lvl, msg) =>
      println(s"$lvl $msg")
  }
}

object DBManager extends (DB.DSL ~> Id) {
  def apply[A](a: DB.DSL[A]) = a match {
    case DB.FindById(id) =>
      println(s"DB Finding $id")
      Xor.right(Map("id" -> id, "name" -> "toto"))
  }
}

//////////////////////////////////////////////////////////////////////////
// Execution
val interpreter: Interpreter[PRG, Id] = Logger :|: DBManager

DBService.findById(XXX).foldMap(interpreter.nat)

```

Deeper sample can be found in [AppSpec](https://github.com/ProjectSeptemberInc/freek/blob/master/src/test/scala/AppSpec.scala)


> By the way, all of that is also called

#### COMPILE-TIME DEPENDENCY INJECTION

;););)

THE END...
