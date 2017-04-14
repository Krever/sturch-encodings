package sturch

import scala.language.higherKinds


object Main {

  trait TL {
    type Out[T <: TL] <: TL
  }

  trait Identity extends TL {
    type Out[T <: TL] = T
  }

  trait Const[A <: TL] extends TL {
    type Out[T <: TL] = A
  }

  trait True extends TL {
    type Out[T <: TL] = Const[T]
  }

  trait False extends TL {
    type Out[T <: TL] = Identity
  }

  trait If[Pred <: TL, IfTrue <: TL, IfFalse <: TL] extends TL {
    type Out[T] = Pred#Out[IfTrue]#Out[IfFalse]
  }

  trait And[]


  import scala.reflect.runtime.universe.{TypeTag, typeOf}
  def print0[A <: TL]()(implicit tag: TypeTag[A]): Unit = println(typeOf[A].etaExpand)


  def main(args: Array[String]): Unit = {
    print0[True]
    print0[If[True, True, False]#Out[Int]]
    print0[If[True, False, True]#Out[Int]]
  }
}
