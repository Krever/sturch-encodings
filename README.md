# sturch-encodings
**S**cala **T**ypelevel Ch**urch** Encodings

## What ???

```
import sturch.nats._
import sturch.bools._

type `2` = Succ[Succ[Zero]]
type `4` = Mult[`2`, `2`]
type `IsTypelevelProgrammingFun?` = True
type Result = If[`IsTypelevelProgrammingFun?`, Plus[`4`, `2`], Zero]
```

## Why ???
For fun of course.

## Seriously...
This repository contains an attempt to encode some structures(e.g. booleans and natural numbers) 
using only types. You can findout more by googling "type lambdas" and "Church encoding".

### What is avilable?
This library is not published anywhere, because I see no point in using it. If thats a problem let me know and I will publish it to sonatype repository.

Inside you can find five packages:
- `sturch` - definition of type lambda, type-to-value-level parser and printer for type lambdas
- `sturch.bools` - encoding of booleans
- `sturch.nats` - encoding of natural numbers
- `sturch.pairs` - encoding of pair data structure(2-sized tuples)
- `sturch.lists` - encoding of list data structure

## How ???

All of the encoding is based on the Type Lambda abstraction which is defined like this:
```scala
  trait TL {
    type Apply[T <: TL] <: TL
  }
```
And some concrete functions can be encoded like this:
```scala
  type Identity = TL {
    type Apply[T <: TL] = T
  }
```

To see more examples you can look into tests.