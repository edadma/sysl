package xyz.hyperreal._scala_native

import utest._

object UnitTests extends TestSuite {

  val tests = Tests {
    test("test 1") {
      assert(1 + 2 == 3)
    }

    test("test 2") {
      val a = "asdf"

      assert(s"$a 123" == "asdf 123")
    }
  }

}
