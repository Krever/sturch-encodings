package sturch

import sturch.bools.{And, False, True}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Created by wpitula on 4/17/17.
  */
object nats {

  type Zero = TL {
    type Apply[F <: TL] = TL {
      type Apply[X <: TL] = X
    }
  }

  type SuccTL = TL {
    type Apply[N <: TL] = TL {
      type Apply[F <: TL] = TL {
        type Apply[X <: TL] = F#Apply[N#Apply[F]#Apply[X]]
      }
    }
  }
  type Succ[Num <: TL] = SuccTL#Apply[Num]

  type `1` = Succ[Zero]
  type `2` = Succ[`1`]
  type `3` = Succ[`2`]
  type `4` = Succ[`3`]
  type `5` = Succ[`4`]

  type PlusTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = TL {
        type Apply[F <: TL] = TL {
          type Apply[X <: TL] = M#Apply[F]#Apply[N#Apply[F]#Apply[X]]
        }
      }
    }
  }
  type Plus[N <: TL, M <: TL] = PlusTL#Apply[N]#Apply[M]

  type MultTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = TL {
        type Apply[F <: TL] = M#Apply[N#Apply[F]]

      }
    }
  }
  type Mult[N <: TL, M <: TL] = MultTL#Apply[N]#Apply[M]

  type ExpTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = N#Apply[M]
    }
  }
  type Exp[M <: TL, N <: TL] = ExpTL#Apply[M]#Apply[N]

  type PredTL = TL {
    type Apply[N <: TL] = TL {
      type Apply[F <: TL] = TL {
        type Apply[X <: TL] = N#Apply[TL {
          type Apply[G <: TL] = TL {
            type Apply[H <: TL] = H#Apply[G#Apply[F]]
          }
        }]#Apply[TL {
          type Apply[U <: TL] = X
        }]#Apply[TL {
          type Apply[V <: TL] = V
        }]
      }
    }
  }
  type Pred[N <:TL] = PredTL#Apply[N]

  type MinusTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = N#Apply[PredTL]#Apply[M]
    }
  }
  type Minus[M <: TL, N <: TL] = MinusTL#Apply[M]#Apply[N ]


  type IsZeroTL = TL {
    type Apply[N <: TL] = N#Apply[Const[False]]#Apply[True]
  }
  type IsZero[N <: TL] = IsZeroTL#Apply[N]

  type LEQTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = IsZero[Minus[M, N]]
    }
  }
  type LEQ[M <: TL, N <: TL] = LEQTL#Apply[M]#Apply[N]
  type `<=`[M <: TL, N  <: TL] = LEQ[M, N]

  type EQTL = TL {
    type Apply[M <: TL] = TL {
      type Apply[N <: TL] = And[LEQ[M, N], LEQ[N, M]]
    }
  }
  type EQ[M <: TL, N <: TL] = EQTL#Apply[M]#Apply[N]
  type `==`[M <: TL, N  <: TL] = EQ[M, N]


  def readInt(expr: Expr): Try[Int] = {
    def countCalls(expr: Expr, fun: Var, x: Var, count: Int = 0): Try[Int] = {
      expr match {
        case Var(a) if a == x.name => Success(count)
        case Apply(Var(f), body) if f == fun.name => countCalls(body, fun, x, count + 1)
        case _ => Failure(new IllegalArgumentException("Not an int"))
      }
    }

    expr match {
      case Lambda(fun, Lambda(x, body)) => countCalls(body, fun, x)
      case _ => Failure(new IllegalArgumentException("Not an int"))
    }
  }

  import scala.reflect.runtime.universe.WeakTypeTag
  def readIntUnsafe[T: WeakTypeTag]: Int = readInt(parse[T]).get

}
