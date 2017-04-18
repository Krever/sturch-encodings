package sturch

/**
  * Created by wpitula on 4/18/17.
  */
object lists {

  import pairs._
  import bools._

  type Nil = Pair[True, True]

  type IsNilTL = FirstTL
  type IsNil[L <: TL] = IsNilTL#Apply[L]

  type ConsTL = TL {
    type Apply[H <: TL] = TL {
      type Apply[T <: TL] = Pair[False, Pair[H, T]]
    }
  }
  type Cons[H <: TL, T <: TL] = ConsTL#Apply[H]#Apply[T]

  type HeadTL = TL {
    type Apply[L <: TL] = First[Second[L]]
  }
  type Head[L <: TL] = HeadTL#Apply[L]

  type TailTL = TL {
    type Apply[L <: TL] = Second[Second[L]]
  }
  type Tail[L <: TL] = TailTL#Apply[L]


}
