package sturch

import org.scalatest.{FunSuite, Matchers}
import scala.reflect.runtime.universe.WeakTypeTag

/**
  * Created by wpitula on 4/18/17.
  */
class boolsSpec extends FunSuite with Matchers {

  import bools._

  test("read bool") {
    readBoolUnsafe[True] shouldBe true
    readBoolUnsafe[False] shouldBe false
  }

  test("and operator") {
    readBoolUnsafe[And[True, True]] shouldBe true
    readBoolUnsafe[And[True, False]] shouldBe false
    readBoolUnsafe[And[False, False]] shouldBe false
    readBoolUnsafe[And[False, True]] shouldBe false
  }

  test("or operator") {
    readBoolUnsafe[Or[True, True]] shouldBe true
    readBoolUnsafe[Or[True, False]] shouldBe true
    readBoolUnsafe[Or[False, False]] shouldBe false
    readBoolUnsafe[Or[False, True]] shouldBe true
  }

  test("not operator") {
   readBoolUnsafe[Not[True]] shouldBe false
   readBoolUnsafe[Not[False]] shouldBe true
  }

  test("if operator") {
    readBoolUnsafe[If[True, True, False]] shouldBe true
    readBoolUnsafe[If[True, False, True]] shouldBe false
    readBoolUnsafe[If[False, True, False]] shouldBe false
    readBoolUnsafe[If[False, False, True]] shouldBe true
  }

}
