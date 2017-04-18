package sturch

/**
  * Created by wpitula on 4/18/17.
  */
object pairs {

  type PairTL = TL {
    type Apply[X <: TL] = TL {
      type Apply[Y <: TL] = TL {
        type Apply[Z <: TL] = Z#Apply[X]#Apply[Y]
      }
    }
  }

  type Pair[X <: TL, Y <: TL] = PairTL#Apply[X]#Apply[Y]

  type FirstTL = TL {
    type Apply[P <: TL] = P#Apply[TL {
      type Apply[X <: TL] = TL {
        type Apply[Y <: TL] = X
      }
    }]
  }
  type First[P <: TL] = FirstTL#Apply[P]

  type SecondTL = TL {
    type Apply[P <: TL] = P#Apply[TL {
      type Apply[X <: TL] = TL {
        type Apply[Y <: TL] = Y
      }
    }]
  }
  type Second[P <: TL] = SecondTL#Apply[P]


}
