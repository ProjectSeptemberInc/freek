# Freek, a freaky simple Free to combine your DSL seamlessly

> Pere Villega wrote a really cool introduction to Freek & Free before I could even move a finger and it's a really good one even if it was relying on previous version of the API but the ideas are still the same :D 
>
> http://perevillega.com/freek-and-free-monads
>
> Thanks a lot Pere


<br/>
<br/>
## Current Version

> All versions are published to bintray https://bintray.com/projectseptemberinc/

<br/>
### v0.6.5:

- first scala 2.12 support
- cross-compiling 2.11.8 & 2.12
- depends on cats 0.8.0

<br/>
### v0.6.1/2:

- added `Freekit`/`Freekito` helpers to reduce `.freek` boilerplate in basic cases
- added `transpile` to flatten Free programs combining Free programs
- added `:&&:` operator to combine group of interpreters

<br/>
### v0.6.0:

- renamed `FX` to `DSL` and `NilDSL` to `NilDSL` to represent better the fact that freek is more general than effects and about manipulating any DSL.
- renamed helped `Program` to `DSL.Make`
- renamed `CoproductK` to `CopK`
- renamed `.onionP[O]` to `.onion[O]`
- introduced `peelRight2 & peelRight2` & `onionT2 & onionT3`
- removed unneeded specific OnionT implicit converters
- build for cats-0.4.1/0.6.1/0.7.0

<br/>
### v0.5.0:

- introduced new model for CoproductK to improve compile-time globally (from O(n) to O(log2(n)))
- introduced `Program[F <: FX]` to help scalac infer the output CoproductK when using the new optimized model
- result type of a program based on `freek[PRG]` is no more `PRG.Cop` but `PRG.Cop` based on new `DSL.Make[PRG]`
- optimized implicit order

<br/>
### v0.4.3:

- introduced `CoproductK.AppendK` to make it robust to combine programs containing sub-programs

<br/>
### v0.4.2:

- beta re-introducing operator `:||:` to combine programs to programs... WIP

<br/>
### v0.4.1:

- evaluating `.freeko[PRG, O]` which is equivalent to `.freek[PRG].onionT[O]`
- renamed `dropRight -> peelRight` and `prepend -> wrap`

<br/>
### v0.4.0:

- replaced `type PRG[A] = (Log :|: KVS :|: File :|: NilDSL).Cop[A]` by `type PRG = Log :|: KVS :|: File :|: NilDSL` to simplify the whole API
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

resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")

libraryDependencies ++= Seq(
  "com.projectseptember"            %% "freek"                        % "0.6.0" // with cats-0.7.0
//  "com.projectseptember"          %% "freek"                        % "0.6.0_cats-0.4.1" // with cats-0.4.1
//  "com.projectseptember"          %% "freek"                        % "0.6.0_cats-0.6.1" // with cats-0.6.1
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
type PRG = Log :|: KVS :|: File :|: NilDSL
val PRG = DSL.Make[PRG]
```

Please note:

- `NilDSL` is required at the end of the coproduct and represents the non-existing DSL
- Some will complain on the ugly symbol `:|:` but in Scala, there is no other elegant way to combine types (words can't be used in this case)...
- `val PRG = DSL.Make[PRG]` is the way to instantiate a value that represents your `PRG` type. It might seem artificial and actually it is completely: it is just required to convince scalac that it can infer the right coproduct combining all your DSL and this inferred Coproduct is represented `PRG.Cop` that will be used in all this tutorial.

<br/>
<br/>
### `.freek[PRG]` to lift all operations in for-comprehension

Now, you want to write a program based on your DSL using Free Monads because it's an efficient way to describe your business logic without supposing how it will be executed.

So, you're going to use your `DSL[_]` lifted into Free monads `Free[DSL[_], A]` using a classic monadic flow i.e. for-comprehension.

In a for-comprehension, to compile successfully, every line should have the same type. Thus, you need to lift all `Free[DSL[_], A]` to the combined Free type `Free[PRG.Cop, A]` where `PRG.Cop` is the sum of all the DSL used in your program.

In a summary, you need a conversion `DSL[A] => Free[DSL, A] => Free[PRG.Cop, A]`.

This can be done in a trivial way using `.freek[PRG]` in your for-comprehension.

```scala
type PRG = Log :|: KVS :|: File :|: NilDSL
val PRG = DSL.Make[PRG]

// Here the type is shown for doc but you can omit it, Scala can infer things
def program(id: String): Free[PRG.Cop, File] = 
  for {
    _     <- Log.debug(s"Searching for value id: $id").freek[PRG]
    name  <- KVS.Get(id).freek[PRG]
    file  <- File.Get(name).freek[PRG]
    _     <- Log.debug(s"Found file:$file").freek[PRG]
  } yield (file)
```

- Every line is lifted by `.freek[PRG]` to `Free[PRG.Cop, A]`: `PRG.Cop` builds the real hidden Sum/Coproduct type combining all your DSL. It is a specialized implementation of Shapeless Coproduct for higher-kinded structures called `CopK` because Shapeless representation doesn't allow to manipulate `F[_]` as we need it.
- Just remind that `PRG` alone is a facility to combine DSL and the real type combining all DSL is `PRG.Cop`.
- The whole for-comprehension describes a program
- The result type is `Free[PRG.Cop, ?]`.

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

- there is no equivalent to `NilDSL` at the end of sequence (because not types but values)
- `interpreter` is actually of type `Interpreter` which is just a wrapper around a `C ~> R`  where `C[_] <: CopK[_]`. If you want to access the underlying NaturalTransformation/FunctionK `PRG ~> Future`, just call `interpreter.nat`.

<br/>
#### Execute your program using `interpret`

`program` is just a `Free[PRG.Cop, A]`, right?

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

  type PRG = Log.DSL :|: DB.DSL :|: NilDSL
  val PRG = DSL.Make[PRG]

  /** the program */
  // Here the type is indicated for doc but you can omit it, Scala can infer things
  def findById(id: String): Free[PRG.Cop, Entity] =
    for {
      _    <- Log.debug("Searching for entity id:"+id).freek[PRG]
      res  <- FindById(id).freek[PRG]
      _    <- Log.debug("Search result:"+res).freek[PRG]
    } yield (res)
}
```

<br/>
#### Combine programs


To prepend one or more DSL to an existing combination of DSL into a new program, use the operator `:|:` also (in the same semantics as `+:` for `Seq`):

```scala

  type PRG = Log :|: KVS :|: File :|: DBService.PRG
  val PRG = DSL.Make[PRG]

  // Here the type is indicated for doc but you can omit it, Scala can infer things
  def program2(id: String): Free[PRG.Cop, File] = 
    for {
      _     <- Log.debug(s"Searching for value id: $id").freek[PRG]
      name  <- KVS.Get(id).freek[PRG]
      e     <- DB.findById(id).freek[PRG]
      file  <- File.Get(e.file).freek[PRG]
      _     <- Log.debug(s"Found file:$file").freek[PRG]
    } yield (file)

```

Please note:

- there is no `NilDSL` at the end because it's brought by `DBService.PRG`
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


<br/>
<br/>
### Manipulate Stack of result types

Ok till now we have focused on the DSL side F of `Free[F[_], A]`. We have nice tools to combine different DSL and interpret them. But what about the result type?

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
type Stack[A] = O#Layers[A]
```

_Bulb is just the terminator of the `Onion` stack (like NilDSL for combination of DSL)_


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
Free[PRG.Cop, Option[A]]
// and
Free[PRG.Cop, Xor[String, A]]

// into
Free[PRG.Cop, Xor[String, Option[A]]]

// which is
type O = Xor[String, ?] :&: Option :&: Bulb
Free[PRG.Cop, O#Layers]
```

As you can expect, that's not enough, you need something more to do what we want.

<br/>
#### OnionT, the missing link

For `Option`, the Monad Transformer is called `OptionT`
For `Xor`, the Monad Transformer is called `XorT`

For `Onion`, Freek provides `OnionT[TC[_[_], _], F[_], O <: Onion, A]`...

Freaky, isn't it? Don't worry, you don't have to see it most of the time like monad transformers :D

> Let's say loud that _`OnionT` is not a Monad Transformer_: is an Onion stack of monadic & traversable layers embedded as result type of a  monad `TC[F[_], ?]` (like `Free[F[_], ?]`).

Finally, if you are able to lift all your `Free[PRG.Cop, Option[A] or Xor[String, A]]` to `OnionT[Free, PRG.Cop, O, A]`, victory!
... And you can do it with `.onionT[O]`

Let's give an example of it:

```scala
type PRG = Bar :|: Foo :|: Log.DSL :|: NilDSL
type O = Xor[String, ?] :&: Option :&: Bulb

// Here the type is indicated for doc but you can omit it, Scala can infer things
val prg: OnionT[Free, PRG.Cop, O, Int] = for {
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

`prg` has type `OnionT[Free, PRG.Cop, O, A]` but you want to execute it as a Free Monad, not this weird OnionT-stuff.

It's as simple as you would do with Monad Transformers: access the underlying Free with `.value`

```scala
val fut = prg.value.interpret(interpreters)
```

<br/>
#### `.freeko[PRG, O]` for the very lazy

Ok, writing `.freek[PRG].onionT[O]` on each line is boring, right?

Freek provides a shortcut called `freeko` that combines both calls in one single.

> `freeko` is still in evaluation as it requires some type crafting so if you see weird cases, don't hesitate to report

Here is your program with `freeko`:

```scala
type PRG = Bar :|: Foo :|: Log.DSL :|: NilDSL
type O = Xor[String, ?] :&: Option :&: Bulb

// Here the type is indicated for doc but you can omit it, Scala can infer things
val prg: OnionT[Free, PRG.Cop, O, Int] = for {
  i     <- Foo1("5").freeko[PRG, O]
  i2    <- Foo2(i).freeko[PRG, O]
  _     <- Log.info("toto " + i).freeko[PRG, O]
  _     <- Foo3.freeko[PRG, O]
  s     <- Bar1(i2.toString).freeko[PRG, O]
  i3    <- Foo4(i2).freeko[PRG, O]
} yield (i3)
```

Ok, that's enough simplifications...

<br/>
#### Combine sub-programs together with `:||:`

Finally, you can combine existing programs together and also define local programs.

[Debasish Gosh](https://twitter.com/debasishg) asked me how I would define local programs & re-use them in bigger programs.

Let's take his classic example from his great book [Functional and Reactive Domain Modeling](https://www.manning.com/books/functional-and-reactive-domain-modeling) that is clearly related to Freek domain.

<br/>
##### Define local programs

```scala
// Repository DSL
sealed trait Repo[A]
case class Query(no: String) extends Repo[Xor[String, Account]]
case class Store(account: Account) extends Repo[Xor[String, Account]]
case class Delete(no: String) extends Repo[Xor[String, Unit]]

// Repository local program
object Repo {
  type PRG = Repo :|: Log :|: NilDSL
  val PRG = DSL.Make[PRG]

  type O = Xor[String, ?] :&: Bulb

  // here you can define a local program re-usable in other programs
  def update(no: String, f: Account => Account) = for {
    a <-  Query(no).freeko[PRG, O]
    _ <-  Store(f(a)).freeko[PRG, O]
  } yield (())
}

sealed trait Foo[A]
...

// Foo can use Repo sub-program
object Foo {
  type PRG = Foo :|: Log.DSL :|: Repo.PRG

  def subFoo(...): Free[PRG.Cop, ...] = ...
}

sealed trait Bar[A]
...

// Bar can use Repo sub-program too
object Bar {
  type PRG = Bar :|: Log.DSL :|: Repo.PRG
  val PRG = DSL.Make[PRG]

  def subBar(...): Free[PRG.Cop, ...] = ...
}
```

You can see that `NilDSL` isn't used in `Bar.PRG` and `Foo.PRG` because `:|:` prepends an element DSL to a (coproduct) sequence of DSL and `Repo.PRG` is already a (coproduct) sequence of DSL.

<br/>
##### Combine programs with `:||:`

```scala
type O = List :&: Xor[String, ?] :&: Option :&: Bulb

// Combine all programs using :||:
type PRG = Log.DSL :|: Bar.PRG :||: Foo.PRG
val PRG = DSL.Make[PRG]

// Here the type is indicated for doc but you can omit it
val prg: OnionT[Free, PRG.Cop, O, Int] = for {
  ...   <- Foo.subFoo(...).freeko[PRG, O]
  ...   <- Bar.subBar(...).freeko[PRG, O]
  _     <- Repo.update(.).freeko[PRG, O]
} yield (...)
```

To make the difference between `:|:` and `:||:`, please remind the following:

- `:|:` is like operator `+:` for Scala `Seq`, it prepends an element DSL to a (coproduct) sequence of DSL.

- `:||:` is like operator `++` for Scala `Seq`, it appends 2 (coproduct) sequences of DSL.

<br/>
#### Combine group of interpreters with `:&&:`

```scala
val fooInterpreters = barInterpreter :&: logInterpreter :|: repoInterpreter
val barInterpreters = fooInterpreter :&: logInterpreter :|: repoInterpreter

val interpreters = fooInterpreters :&&: barInterpreters
```

- `:&&:` is like operator `++` for Scala `Seq`, it appends 2 sequences of interpreters.

<br/>
#### Unstack results with `.peelRight` / `.peelRight2` / `.peelRight3` 

Sometimes, you have a Free returning an Onion `Xor[String, ?] :&: Option :&: Bulb` but you want to manipulate the hidden `Option[A]` in your program and not `A`.

You can do that using `.dropRight` that will unstack `Option` from the onion `Xor[String, ?] :&: Option :&: Bulb` and return a `(Xor[String, ?] :&: Bulb)#Layers[Option[A]]`. Then you have access to `Option[A]` but naturally, you have to lift all lines of the program to the same level.

For example, you could do the following:

```scala
val prg: OnionT[Free, PRG.Cop, Xor[String, ?] :&: Bulb, Option[A]] = for {
  iOpt  <-  Foo1("5").freek[PRG].onionT[O].peelRight
  i2    <-  iOpt match {
              case Some(i) => Foo2(i).freek[PRG].onionT[O].peelRight
              case None => Foo2(0).freek[PRG].onionT[O].peelRight
            }
  ...
}
```

- If you need to peel 2 layers, use `.peelRight2`

- If you need to peel 3 layers, use `.peelRight3`

> there is also a `.wrap[F[_]]` that can wrap the existing Onion in `F[_]`

<br/>
#### Difference between `.onionT` and `.onion`

- When you have a `Free[PRG.Cop, F[A]]` and want to lift info `OnionT[Free, PRG.Cop, O, A]` and Onion `O` contains `F[_]`, then use `Free[PRG.Cop, F[A]].onionT[O]`


- When you have a `Free[PRG.Cop, F[A]]` into `OnionT[Free, PRG.Cop, O, F[A]]` (Onion `O` can contain `F[_]`), then use `Free[PRG.Cop, F[A]].onion[O]`

#### `.onionT1` / `.onionT2` / `.onionT3`

Instead of `Foo2(i).freek[PRG].onionT[O].peelRight`, you can write `Foo2(i).freek[PRG].onionT1[O]`

Instead of `Foo2(i).freek[PRG].onionT[O].peelRight2`, you can write `Foo2(i).freek[PRG].onionT2[O]`

Instead of `Foo2(i).freek[PRG].onionT[O].peelRight3`, you can write `Foo2(i).freek[PRG].onionT3[O]`

<br/>
#### Bored adding `.free[PRG]` on each line? Use `Freekit` trick

```
type PRG = Foo1 :|: Foo2 :|: Log :|: NilDSL
val PRG = DSL.Make[PRG]

// remark that you pass the value PRG here
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
```

This works in basic cases & naturally as soon as you have embedded `for-comprehension`, scalac inference makes it less efficient.

<br/>
#### Bored adding `.free[PRG].onionT[O]` on each line? Use `Freekito` trick

```
type PRG = Foo1 :|: Foo2 :|: Log :|: NilDSL
val PRG = DSL.Make[PRG]

// remark that you pass the value PRG here
object MO extends Freekito(PRG) {
  // you need to create this type O to reify the Onion
  type O = Option :&: Bulb 

  val prg = for {
    a    <- Foo1.Bar1(7)
    _    <- Log.Info(s"a:$a")
    a    <- Foo2.Bar21(a)
  } yield (a)
}
```

This works in basic cases & naturally as soon as you have embedded `for-comprehension`, scalac inference makes it less efficient.


<br/>
<br/>
## Reminding motivations

At [ProjectSeptember](http://www.projectseptember.com), we love typesafe & functional programming.

We also like the concept of `Free Monad` which decouples completely the description of your program from its execution.

Free has a cost in terms of performance & short-term structures allocation but in our domain, IO is much more the bottleneck than these aspects so we can use those concepts without remorse.

We also believe current implementations can be improved progressively using theoretical and even more brutal tools like compiler plugin/optimizations. So we want to push those concepts further and further.

In Cats & Scalaz, `Free[F[_], A]` representation has already been optimized into a right associated structure and now embeds Coyoneda trick removing the dependency on Functor. Now we can use any DSL `F[_]` in a `Free[F, A]`.

Free is a generic way to manipulate DSL and convert them into computations and thus can be used to represent some sort of effects combination. But in this context, there are more specialized & interesting approaches in Scala world:

- [Emm](https://github.com/djspiewak/emm) : A very clever idea to represent stack of effects and a nice implementation (from which a few ideas have been stolen for `freek`). It's interesting because it can use existing structures but it has many implicits meaning it has a cost at compile-time and a few questioning about its pure Monadic nature (TBD)...

- [Scala Eff](http://atnos-org.github.io/eff-cats/): a more theoretical and deeply interesting implementation based on [Freer Monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)'s paper by Oleg Kiselyov & friends. It's the next-gen of effects management but it also requires more _aware developers_ certainly... Next step of evangelization ;)... Please note that Eric has recently introduced a model in Eff that got inspired by our latest compile-time optimized CopK model: that's the power of OSS ;)

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
