# Freek, a freaky simple Free to combine your DSL seamlessly

> Pere Villega wrote a really cool introduction to Freek & Free before I could even move a finger and it's a really good one even if it was relying on previous version of the API but the ideas are still the same :D 
>
> http://perevillega.com/freek-and-free-monads
>
> Thanks a lot Pere


<br/>
<br/>
## Current Version

<br/>
### v0.4.0:

- replaced `type PRG[A] = (Log :|: KVS :|: File :|: FXNil)#Cop[A]` by `type PRG = Log :|: KVS :|: File :|: FXNil` to simplify the whole API
- introduced OnionT manipulations `.dropRight`, `.prepend`

<br/>
### v0.3.0:

- replaces Freenion by OnionT which generalized the Onion embedding to any structure of type `TC[_[_], _]` and not just Free
- provides new `interpret` function solving order/redundancy issues with DSL vs interpreters
- renames Interpreter combining operator `:|:` into `:&:` because combining interpreters is about a product between them, not a sum of them

## Freek: a freaky simple Free to combine your DSL seamlessly

Freek is just a few helpers & tricks to make it straightforward to manipulate Free & DSL without hiding it's a Free.

> This project has developed for [ProjectSeptember](http://www.projectseptember.com) which has kindly accepted that we opensource it under Apache2 License to make it as open & free as possible... Contributions & comments are welcome...

<br/>
<br/>
## Freek out-of-the-box

<br/>
### Use it in your project

```
# in build.sbt

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("projectseptember", "maven")

libraryDependencies ++= Seq(
  "com.projectseptember"            %% "freek"                        % "0.4.0"
, "org.spire-math"                  %% "kind-projector"               % "0.7.1"
, "com.milessabin"                  %% "si2712fix-plugin"             % "1.2.0"
)
```

```
# in plugins.sbt
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
```

`KindProjector` plugin isn't required but it makes higher-kinded types so nicer that I can't do anything else than advising to use it.

`si2712fix-plugin` is required or you'll have weird compiling errors... Scalac 2.11 is not powerful enough to unify types in a correct as we would expect for Freek. But Scala 2.12 will be better as si2712fix has been merged into it thanks to Miles Sabin fantastic work.

> Don't worry using this plugin seriously because it does it's job just at compile-time helping scalac to infer types in the right but it won't have any impact on runtime so no worry in any case.


<br/>
<br/>
### Combine your DSL(s) with operator `:|:` (OR)

Imagine you have the following DSL(s):

```scala
sealed trait Log[A]
case class LogMsg(level: LogLevel, msg: String) extends Log[Unit]
object Log {
  def debug(msg: String) = LogMsg(DebugLevel, msg)
  def info(msg: String) = LogMsg(InfoLevel, msg)
}

sealed trait KVS[A]
object KVS {
  final case class Get(key: String) extends KVS[String]
  final case class Put(key: String, value: String) extends KVS[Unit]
}

sealed trait FileIO[A]
object FileIO {
  final case class GetFile(name: String) extends FileIO[File]
  final case class DeleteFile(name: String) extends FileIO[Unit]
  ...
}
```

You want to build a program that manipulates the 3 DSL together.
So this program will use Log `or` KVS `or` File which is a Sum/Coproduct of the 3 DSL.

To represent the DSL summing them all, Freek provides you with the following notation:

```scala
type PRG = Log :|: KVS :|: File :|: FXNil
```

Please note:

- `FXNil` is required at the end of the coproduct
- Some will complain on the ugly symbol `:|:` but in Scala, there is no other elegant way to combine types...

> in version <0.4.0', `#Cop[A]` was required to build the real hidden Sum/Coproduct type combining all DSL but after a remark from Daniel Spiewak, it has appeared to be useless

<br/>
<br/>
### `.freek[PRG]` to lift all operations in for-comprehension

Now, you want to write a program based on your DSL using Free Monads because it's an efficient way to describe your business logic without supposing how it will be executed.

So, you're going to use your `DSL[_]` lifted into Free monads `Free[DSL[_], A]` using a classic monadic flow i.e. for-comprehension.

In a for-comprehension, to compile successfully, every line should have the same type. Thus, you need to lift all `Free[DSL[_], A]` to the common Free type `Free[PRG, A]` where `PRG` is the sum of all the DSL used in your program.

In a summary, you need a conversion `DSL[A] => Free[DSL, A] => Free[PRG, A]`.

This can be done in a trivial way using `.freek[PRG]` in your for-comprehension.

```scala
type PRG = Log :|: KVS :|: File :|: FXNil

def program(id: String) = 
  for {
    _     <- Log.debug(s"Searching for value id: $id").freek[PRG]
    name  <- KVS.Get(id).freek[PRG]
    file  <- File.Get(name).freek[PRG]
    _     <- Log.debug(s"Found file:$file").freek[PRG]
  } yield (file)
```

- Every line is lifted by `.free[PRG]` to `Free[PRG#Cop, A]`: `PRG#Cop` builds the real hidden Sum/Coproduct type combining all your DSL. It is a specialized implementation of Shapeless Coproduct for higher-kinded structures called `CoproductK` because Shapeless one doesn't allow to manipulate `F[_]` as we need it.
- Just remind that `PRG` alone is a facility to combine DSL and the real type combining all DSL is `PRG#Cop`.
- The whole for-comprehension describes a program

> Some people will think about a implicit conversion to avoid having to write `freek[PRG]` but believe my own experience, inference in for-comprehension isn't so logical in Scala and as soon as you manipulate more complex programs, implicit conversion makes inference break with hardly understandable errors.

<br/>
<br/>
### Combine interpreters using operator `:&:` (AND)

Previous `program` just describes your sequence of operations but it doesn't suppose how it will be executed: it's just a data representation of your program, a description of your computation.

Now, you need to _interpret_ this description into an effectful execution and this is done with `interpreters` in Freek.
`Interpreters` in Freek are nothing else than classic Natural Transformation (aka FunctionK) `F[_] ~> G[_]` with some helpers to manipulate multiple combined DSL(s) defined above seamlessly.

Let's suppose we use an async execution context based on Scala `Future` not to be too fancy.

<br/>
#### Define your interpreters per DSL

```scala
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

<br/>
#### Combine interpreters into a big interpreter with `:&:`

Executing your `program` means you are able to interpret DSL `Log` and `KVS` and `File` into an effectful computation.

So you need an interpreter which is the product of `KVSInterpreter` and `LogInterpreter` and `FileInterpreter`.

In Freek, you can combine your interpreters using operator `:&:` (AND)

```scala
val interpreter = KVSInterpreter :&: LogInterpreter :&: FileInterpreter
```

Remark that:

- there is no equivalent to `FXNil` at the end of sequence (because not types but values)
- `interpreter` is actually of type `Interpreter` which is just a wrapper around a `C ~> R`  where `C[_] <: CoproductK[_]`. If you want to access the underlying NaturalTransformation/FunctionK `PRG ~> Future`, just call `interpreter.nat`.

<br/>
#### Execute your program using `interpret`

`program` is just a `Free[PRG#Cop, A]`, right?

So you could use simply `foldMap/compile` with your `interpreter.nat`.

But Freek provides a smarter function called `interpret` that makes the order of DSL vs interpreters not relevant as it will be shown further in this documentation.

```scala
val fut = program.interpret(interpreter) // this returns a Future[Unit]
```

<br/>
<br/>
### Combine programs together with same operator `:|:` (SUPER-OR)

The big interest of Free programs is that you can call a Free program inside a Free program. In this case, logically, you need to combine the DSL of both programs into one single DSL and lift all your Free to this bigger DSL.

<br/>
#### Introduce new DSL & program

```scala
// New DSL
object DB {

  sealed trait DSL[A]
  case class FindById(id: String) extends DSL[Entity]

}

// the new program
object DBService {
  import DB._

  type PRG = Log.DSL :|: DB.DSL :|: FXNil

  /** the program */
  def findById(id: String): Free[PRG, Entity] =
    for {
      _    <- Log.debug("Searching for entity id:"+id).freek[PRG]
      res  <- FindById(id).freek[PRG]
      _    <- Log.debug("Search result:"+res).freek[PRG]
    } yield (res)
}
```

<br/>
#### Combine programs

To combine an existing combination of DSL into a new program, use the operator `:||:` (2x`|`):

```scala

  type PRG = Log :|: KVS :|: File :|: DBService.PRG

  def program2(id: String) = 
  for {
    _     <- Log.debug(s"Searching for value id: $id").freek[PRG]
    name  <- KVS.Get(id).freek[PRG]
    e     <- DB.findById(id).freek[PRG]
    file  <- File.Get(e.file).freek[PRG]
    _     <- Log.debug(s"Found file:$file").freek[PRG]
  } yield (file)

```

Please note:

- there is no `FXNil` at the end because it's brought by `DBService.PRG`
- `:|:` also appends a list of DSL at the end


<br/>
#### What about DSL redundancy & order?

You have remarked that `Log` is redundant in `PRG & `DBService.PRG` so it might be an issue when declaring your sequence of interpreters. You might also wonder what happens if you don't respect order of DSL in the sequence of interpreters.

If you were using classic `foldMap/compile` to execute your Free program, you would have to respect the exact order and redundancy of interpreters vs DSL which is a pain.

This is where Freek `interpret` really becomes interesting as it is order & redundancy-agnostic.

So, for previous combined programs, you can just do:

```scala

val interpreter2 = DBInterpreter :&: KVSInterpreter :&: LogInterpreter :&: FileInterpreter

val fut2 = program2.interpret(interpreter2) // this returns a Future[Unit]
```

> `:&&:` operator that could merge 2 interpreters together doesn't exist but show come soon


<br/>
<br/>
### Manipulate Stack of result types

Ok till now we have focused on the DSL/effect side F of `Free[F[_], A]`. We have nice tools to combine different effects/DSL and interpret them. But what about the result type?

In previous samples, we had simple return types in our DSL but this return type often represents the results of operations in your business logic including error types.

Let's define this kind of DSL:


```scala
sealed trait Foo[A]
final case class Foo1(s: String) extends Foo[Option[Int]]
final case class Foo2(i: Int) extends Foo[Xor[String, Int]]
final case object Foo3 extends Foo[Unit]
final case class Foo4(i: Int) extends Foo[Xor[String, Option[Int]]]

sealed trait Bar[A]
final case class Bar1(s: String) extends Bar[Option[String]]
final case class Bar2(i: Int) extends Bar[Xor[String, String]]
```

Here you see some return type `Option[Int]` or `Xor[String, Int]`.

In general, you want to write programs like:

```scala
for {
  i <- Foo1("5").freek[PRG]                    // => here result is Option[String]
  _ <- Bar2(i).freek[PRG]                      // => here result is Xor[String, String]
  ...
} yield (())
```

But you see clearly that `i` is an Option and types on both lines aren't the same.

If you try to solve those naively in your program, you'll end with something like that:

```scala
for {
  optI <- Foo1("5").freek[PRG]                    // => here result is Option[String]
  _    <- optI match {
            case Some(i) => Bar2(i).freek[PRG]    // => here result is Xor[String, String]
            case None => ...                      // => What to return here???
          }
  ...
} yield (())
```

It's ugly and still needs to unify Xor[String, String] and Option[String] in a common type.

The natural approach is to stack your complex return types to be able to manage all cases (order matters naturally):

```scala
Xor[String, Option[String]] or Option[Xor[String, String]]

// orders matters naturally and depends on your business rules
```

As you may know, the classic approach to this is to use _MonadTransformers_ (`OptionT` & `XorT`) which work well with Freek but Freek also provides new typesafe facilities called `Onion` & `OnionT` to make it a bit more trivial.

<br/>
#### Onion, stack of monads/traverses

`Onion` is just a way to manipulate a stack of monadic & traversable structures like:

```scala
type Stack[A] = F[G[H[I[A]]]] (where F/G/H/I must be monads & traversables to allow all features provided by Onion)
```

You can represent it using `Onion` facilty:

```scala
// Build your Onion like that
type O = F :&: G :&: H :&: I :&: Bulb

// then you could get the Stack again (but in general you don't need it)
type Stack[A] = O#Build[A]
```

_Bulb is just the terminator of the `Onion` stack (like FXNil for stack of effects/DSL)_


Let's go back to our original program now and try type unification on every line:


```scala
for {
  i   <- Foo1("5").freek[PRG] // => here result is Option[String]
  s   <- Bar2(i).freek[PRG]   // => here result is Xor[String, String]
  ...
} yield (())
```

So, in the for-comprehension, we need to unify types on every line:

```scala
Free[PRG#Cop, Option[A]]
// and
Free[PRG#Cop, Xor[String, A]]

// into
Free[PRG#Cop, Xor[String, Option[A]]]

// which is
type O = Xor[String, ?] :&: Option :&: Bulb
Free[PRG#Cop, O#Build]
```

As you can expect, that's not enough, you need something more to do what we want.

<br/>
#### OnionT, the missing link

For `Option`, the Monad Transformer is called `OptionT`
For `Xor`, the Monad Transformer is called `XorT`

For `Onion`, Freek provides `OnionT[TC[_[_], _], F[_], O <: Onion, A]`...

Freaky, isn't it? Don't worry, you don't have to see it most of the time like monad transformers :D

> Let's say loud that _`OnionT` is not a Monad Transformer_: is an Onion stack of monadic & traversable layers embedded as result type of a  monad `TC[F[_], ?]` (like `Free[F[_], ?]`).

Finally, if you are able to lift all your `Free[PRG, Option[A] or Xor[String, A]]` to `OnionT[Free, PRG, O, A]`, victory!
... And you can do it with `.onionT[O]`

Let's give an example of it:

```scala
type PRG = Bar :|: Foo :|: Log.DSL :|: FXNil
type O = Xor[String, ?] :&: Option :&: Bulb

val prg = for {
  i     <- Foo1("5").freek[PRG].onionT[O]
  i2    <- Foo2(i).freek[PRG].onionT[O]
  _     <- Log.info("toto " + i).freek[PRG].onionT[O]
  _     <- Foo3.freek[PRG].onionT[O]
  s     <- Bar1(i2.toString).freek[PRG].onionT[O]
  i3    <- Foo4(i2).freek[PRG].onionT[O]
} yield (i3)
```

Remark that `.oniontT[O]` is used in all cases to lift to `OnionT[Free, PRG, O, A]`

<br/>
#### Execute an OnionT with `.value`

`prg` has type `OnionT[Free, PRG#Cop, O, A]` but you want to execute it as a Free Monad, not this weird OnionT-stuff.

It's as simple as you would do with Monad Transformers: access the underlying Free with `.value`

```scala
val fut = prg.value.interpret(interpreters)
```

<br/>
#### Unstack results with `.peelRight`

Sometimes, you have a Free returning an Onion `Xor[String, ?] :&: Option :&: Bulb` but you want to manipulate the hidden `Option[A]` in your program and not `A`.

You can do that using `.dropRight` that will unstack `Option` from the onion `Xor[String, ?] :&: Option :&: Bulb` and return a `(Xor[String, ?] :&: Bulb)#Build[Option[A]]`. Then you have access to `Option[A]` but naturally, you have to lift all lines of the program to the same level.

For example, you could do the following:

```scala
val prg = for {
  iOpt  <-  Foo1("5").freek[PRG].onionT[O].peelRight
  i2    <-  iOpt match {
              case Some(i) => Foo2(i).freek[PRG].onionT[O].peelRight
              case None => Foo2(0).freek[PRG].onionT[O].peelRight
            }
  ...
}
```

> there is also a `.prepend[F[_]]` that can prepend a F to an existing Onion O


<br/>
<br/>
## Reminding motivations

At [ProjectSeptember](http://www.projectseptember.com), we love typesafe & functional programming.

We also like the concept of `Free Monad` which decouples completely the description of your program from its execution.

Free has a cost in terms of performance & short-term structures allocation but in our domain, IO is much more the bottleneck than these aspects so we can use those concepts without remorse.

We also believe current implementations can be improved progressively using theoretical and even more brutal tools like compiler plugin/optimizations. So we want to push those concepts further and further.

In Cats & Scalaz, `Free[F[_], A]` representation has already been optimized into a right associated structure and now embeds Coyoneda trick removing the dependency on Functor. Now we can use any effect/DSL `F[_]` in a `Free[F, A]`.

Free is often associated to effects management and in this context, there are also interesting newer approaches in Scala world:

- [Emm](https://github.com/djspiewak/emm) : A very clever idea to represent stack of effects and a nice implementation (from which a few ideas have been stolen for `freek`). It's interesting because it can use existing structures but it has many implicits meaning it has a cost at compile-time and a few questioning about its pure Monadic nature (TBD)...

- [Scala Eff](http://atnos-org.github.io/eff-cats/): a more theoretical and deeply interesting implementation based on [Freer Monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)'s paper by Oleg Kiselyov & friends. It's the next-gen of effects management but it also requires more _aware developers_ certainly... Next step of evangelization ;)

- [Idris Eff port](https://github.com/mandubian/scalaeff): this is my personal toy... In Idris, it's (almost) nice, in Scala, it's almost a monster and more an experiment showing it could work. But it's the _next-gen + 1/2/3/4_ IMHO so let's be patient and make it grow...

> But for now, `Free` concept starts to enter in mind of people so we want to use it as is (with a few enhancements).

<br/>
<br/>
## Tribute to SI2712 patch

SI2712 recent patch released by @milessabin has changed a lot the way we can build type-intensive libraries because we aren't limited by this terrible issue.

> That [PR](https://github.com/scala/scala/pull/5102#issuecomment-219868111) has been merged in Scala 2.12 and it changes things a lot...

Without it, Freek would be much uglier & less fun to use.

Thanks @milessabin again!

<br/>
Deeper sample can be found in [AppSpec](https://github.com/ProjectSeptemberInc/freek/blob/master/src/test/scala/AppSpec.scala)


> By the way, all of that is also called

#### COMPILE-TIME DEPENDENCY INJECTION

;););)

THE END...
