package sturch

import scala.language.higherKinds


object Main {

  type Identity = TL {
    type Apply[T <: TL] = T
  }
  type Const[A <: TL] = TL {
    type Apply[T <: TL] = A
  }
  type True = TL {
    type Apply[T <: TL] = Const[T]
  }
  type False = TL {
    type Apply[T <: TL] = Identity
  }
  type If = TL {
    type Apply[Pred <: TL] = TL {
      type Apply[IfTrue <: TL] = TL {
        type Apply[IfFalse <: TL] = Pred#Apply[IfTrue]#Apply[IfFalse]
      }
    }
  }
  type IfAux[Pred <: TL, IfTrue <: TL, IfFalse <: TL] = If#Apply[Pred]#Apply[IfTrue]#Apply[IfFalse]
  type And = TL {
    type Apply[P <: TL] = TL {
      type Apply[Q <: TL] = P#Apply[Q]#Apply[P]
    }
  }
  type AndAux[P <: TL, Q <: TL] = And#Apply[P]#Apply[Q]
  type Or = TL {
    type Apply[P <: TL] = TL {
      type Apply[Q <: TL] = P#Apply[P]#Apply[Q]
    }
  }
  type OrAux[P <: TL, Q <: TL] = Or#Apply[P]#Apply[Q]
  type Not = TL {
    type Apply[P <: TL] = TL {
      type Apply[A <: TL] = TL {
        type Apply[B <: TL] = P#Apply[B]#Apply[A]
      }
    }
  }
  type NotAux[P <: TL] = Not#Apply[P]
  type Zero = TL {
    type Apply[F <: TL] = Identity
  }
  type Succ = TL {
    type Apply[N <: TL] = TL {
      type Apply[F <: TL] = TL {
        type Apply[X <: TL] = F#Apply[N#Apply[F]#Apply[X]]
      }
    }
  }
  type SuccAux[Num <: TL] = Succ#Apply[Num]

  type Plus = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = TL {
        type Apply[F <: TL] = TL {
          type Apply[X <: TL] = M#Apply[F]#Apply[N#Apply[F]#Apply[X]]
        }
      }
    }
  }
  type PlusAux[N <: TL, M <: TL] = Plus#Apply[N]#Apply[M]

  type `2` = SuccAux[SuccAux[Zero]]
  type `3` = SuccAux[SuccAux[SuccAux[Zero]]]

//  implicit def d2: Describe[`2`] = succDesc[SuccAux[Zero]]
//  implicit def e2: Eval[`2`] = succEval[SuccAux[Zero]]
//  implicit def d3: Describe[`3`] = succDesc[SuccAux[SuccAux[Zero]]]
//  implicit def e3: Eval[`3`] = succEval[SuccAux[SuccAux[Zero]]]

  def main(args: Array[String]): Unit = {
    print[True]
    print[IfAux[True, True, False]]
    print[IfAux[False, True, False]]
    print[AndAux[True, False]]
    print[AndAux[True, True]]
    print[AndAux[False, True]]
    print[AndAux[False, False]]
    print[OrAux[True, False]]
    print[OrAux[True, True]]
    print[OrAux[False, True]]
    print[OrAux[False, False]]
    print[NotAux[True]]
    print[NotAux[False]]
    print[SuccAux[SuccAux[Zero]]]
    print[PlusAux[`2`, `3`]]

  }

  import scala.reflect.runtime.universe._

  def print[A <: TL](implicit describe: Describe[A], eval: Eval[A], tag: TypeTag[A]): Unit =
    println(typeOf[A].toString.replace("sturch.Main.", "").replace("Aux", "") + " = " + eval.show)

  /**
    * Type Lambda
    */
  trait TL {
    type Apply[T <: TL] <: TL
  }

  case class Describe[A <: TL](show: String)

  implicit val trueDesc: Describe[True] = Describe[True]("True")
  implicit val falseDesc: Describe[False] = Describe[False]("False/0")

  implicit def succDesc[Num <: TL](implicit predD: Describe[Num]): Describe[SuccAux[Num]] =
    Describe[SuccAux[Num]](s"succ(${predD.show})")

  implicit def plusDesc[N <: TL, M <: TL](implicit nD: Describe[N], mD: Describe[M]): Describe[PlusAux[N, M]] =
    Describe[PlusAux[N, M]](s"(${nD.show} + ${mD.show})")

  case class Eval[A <: TL](show: String)

  implicit val trueEval: Eval[True] = Eval[True]("True")
  implicit val falseEval: Eval[False] = Eval[False]("0")

  implicit def succEval[Num <: TL](implicit predD: Eval[Num]): Eval[SuccAux[Num]] =
    Eval[SuccAux[Num]]((predD.show.toInt + 1).toString)

  implicit def plusEval[N <: TL, M <: TL](implicit nD: Eval[N], mD: Eval[M]): Eval[PlusAux[N, M]] =
    Eval[PlusAux[N, M]]((nD.show.toInt + nD.show.toInt).toString)


}
