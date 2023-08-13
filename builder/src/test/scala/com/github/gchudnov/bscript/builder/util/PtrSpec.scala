package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.TestSpec

final class PtrSpec extends TestSpec:

  "PtrSpec" when {
    "two instances of case classes are wrapped" should {
      "produce two entries in a Map" in {
        case class A(x: Int)

        val x = A(1)
        val y = A(1)

        val m = Map(Ptr(x) -> "10", Ptr(y) -> "20")

        m.size mustBe (2)
        m.get(Ptr(x)) mustBe Some("10")
        m.get(Ptr(y)) mustBe Some("20")
      }
    }
  }
