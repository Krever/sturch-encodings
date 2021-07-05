package sturch

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
  * Created by wpitula on 4/18/17.
  */
class natsSpec extends AnyFunSuite with Matchers {

  import nats._


  test("zero") {
    readIntUnsafe[Zero] shouldBe 0
  }

  test("successor") {
    readIntUnsafe[Succ[Zero]] shouldBe 1
    readIntUnsafe[Succ[Succ[Zero]]] shouldBe 2
  }

  test("plus operator") {
    readIntUnsafe[Plus[`1`, Zero]] shouldBe 1
    readIntUnsafe[Plus[Zero, `1`]] shouldBe 1
    readIntUnsafe[Plus[`2`, `1`]] shouldBe 3
  }

  test("multipliciation") {
    readIntUnsafe[Mult[`1`, Zero]] shouldBe 0
    readIntUnsafe[Mult[Zero, `1`]] shouldBe 0
    readIntUnsafe[Mult[`1`, `1`]] shouldBe 1
    readIntUnsafe[Mult[`2`, `1`]] shouldBe 2
    readIntUnsafe[Mult[`2`, `2`]] shouldBe 4
  }

  /**
    * This is ignored, because current parser cannot handle [[Exp]] type
    */
  ignore("exponent") {
    readIntUnsafe[Exp[Zero, Zero]] shouldBe 1
    readIntUnsafe[Exp[`2`, Zero]] shouldBe 1
    readIntUnsafe[Exp[`1`, `2`]] shouldBe 1
    readIntUnsafe[Exp[`2`, `1`]] shouldBe 2
    readIntUnsafe[Exp[`2`, `2`]] shouldBe 4
  }

  test("predecessor") {
    readIntUnsafe[Pred[Zero]] shouldBe 0
    readIntUnsafe[Pred[`1`]] shouldBe 0
    readIntUnsafe[Pred[`2`]] shouldBe 1
    readIntUnsafe[Pred[Succ[`2`]]] shouldBe 2
  }

  test("minus operator") {
    readIntUnsafe[Minus[Zero, `1`]] shouldBe 0
    readIntUnsafe[Minus[`1`, Zero]] shouldBe 1
    readIntUnsafe[Minus[`1`, `1`]] shouldBe 0
    readIntUnsafe[Minus[`2`, `1`]] shouldBe 1
    readIntUnsafe[Minus[`2`, Zero]] shouldBe 2
  }

  test("predicates") {
    import bools._
    readBoolUnsafe[IsZero[Zero]] shouldBe true
    readBoolUnsafe[IsZero[`1`]] shouldBe false


    readBoolUnsafe[LEQ[`1`, `2`]] shouldBe true
    readBoolUnsafe[LEQ[`1`, `1`]] shouldBe true
    readBoolUnsafe[LEQ[`2`, `1`]] shouldBe false

    readBoolUnsafe[EQ[`2`, `2`]] shouldBe true
    readBoolUnsafe[EQ[`1`, `2`]] shouldBe false
    readBoolUnsafe[EQ[`2`, `1`]] shouldBe false
    readBoolUnsafe[EQ[`3`, `2`]] shouldBe false
    readBoolUnsafe[EQ[`2`, `3`]] shouldBe false
  }

}
