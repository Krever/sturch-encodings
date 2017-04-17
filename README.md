# sturch-encodings
**S**cala **T**ypelevel Ch**urch** Encodings

## What ???

```
type `2` = Succ[Succ[Zero]]
type `4` = Succ[Succ[`2`]]
type `IsTypelevelProgrammingFun?` = True
type Result = If[`IsTypelevelProgrammingFun?`, Plus[`4`, `2`], Zero]
```

## Why ???
For fun of course.

## Seriously...
This repository contains an attempt to encode some structures(e.g. booleans and natural numbers) 
using only types. You can findout more by googling "type lambdas" and "Church encoding".