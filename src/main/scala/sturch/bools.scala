package sturch

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Created by wpitula on 4/17/17.
  */
object bools {

  type True = TL {
    type Apply[A <: TL] = TL {
      type Apply[B <: TL] = A
    }
  }

  type False = TL {
    type Apply[A <: TL] = TL {
      type Apply[B <: TL] = B
    }
  }

  type IfTL = TL {
    type Apply[Pred <: TL] = TL {
      type Apply[IfTrue <: TL] = TL {
        type Apply[IfFalse <: TL] = Pred#Apply[IfTrue]#Apply[IfFalse]
      }
    }
  }
  type If[Pred <: TL, IfTrue <: TL, IfFalse <: TL] = IfTL#Apply[Pred]#Apply[IfTrue]#Apply[IfFalse]

  type AndTL = TL {
    type Apply[P <: TL] = TL {
      type Apply[Q <: TL] = P#Apply[Q]#Apply[P]
    }
  }
  type And[P <: TL, Q <: TL] = AndTL#Apply[P]#Apply[Q]

  type OrTL = TL {
    type Apply[P <: TL] = TL {
      type Apply[Q <: TL] = P#Apply[P]#Apply[Q]
    }
  }
  type Or[P <: TL, Q <: TL] = OrTL#Apply[P]#Apply[Q]


  type NotTL = TL {
    type Apply[P <: TL] = TL {
      type Apply[A <: TL] = TL {
        type Apply[B <: TL] = P#Apply[B]#Apply[A]
      }
    }
  }
  type Not[P <: TL] = NotTL#Apply[P]

  def readBool(expr: Expr): Try[Boolean] = expr match {
    case Lambda(Var(a), Lambda(Var(b), Var(c))) =>
      if(a == c) Success(true)
      else if (b == c) Success(false)
      else Failure(new IllegalArgumentException("Not a boolean"))
    case _ => Failure(new IllegalArgumentException("Not a boolean"))
  }

}
