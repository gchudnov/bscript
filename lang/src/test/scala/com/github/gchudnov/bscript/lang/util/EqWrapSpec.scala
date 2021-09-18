package com.github.gchudnov.bscript.lang.util

import com.github.gchudnov.bscript.lang.TestSpec

final class EqWrapSpec extends TestSpec:

  "EqWrap" when {
    "two instances of case classes are wrapped" should {
      "produce two entries in a Map" in {
        case class A(x: Int)

        val x = A(1)
        val y = A(1)

        val m = Map(EqWrap(x) -> "10", EqWrap(y) -> "20")

        m.size mustBe (2)
        m.get(EqWrap(x)) mustBe Some("10")
        m.get(EqWrap(y)) mustBe Some("20")
      }
    }
  }
