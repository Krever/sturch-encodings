package sturch

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by wpitula on 4/18/17.
  */
class pairsSpec extends FunSuite with Matchers {

  test("pairs"){
    import pairs._
    import nats._

    type `1` = Succ[Zero]
    type `2` = Succ[`1`]
    type `1and2` = Pair[`1`, `2`]

    readInt(parse[First[`1and2`]]).get shouldBe 1
    readInt(parse[Second[`1and2`]]).get shouldBe 2
  }

}
