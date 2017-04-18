package sturch

import org.scalatest.{FunSuite, Matchers}
import sturch.lists.{Cons, Head, Nil, Tail}
import sturch.nats.{Plus, Succ, Zero, readInt}

/**
  * Created by wpitula on 4/18/17.
  */
class listsSpec extends FunSuite with Matchers {

  test("lists") {
    type `2` = Succ[Succ[Zero]]
    type `3` = Succ[`2`]
    type `4` = Plus[`2`, `2`]

    type myList = Cons[`2`, Cons[`3`, Cons[`4`, Nil]]]

    readInt(parse[Head[myList]]).get shouldBe 2
    readInt(parse[Head[Tail[myList]]]).get shouldBe 3
    readInt(parse[Head[Tail[Tail[myList]]]]).get shouldBe 4
  }
}
