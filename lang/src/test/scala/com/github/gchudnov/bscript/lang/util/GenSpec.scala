package com.github.gchudnov.bscript.lang.util

import com.github.gchudnov.bscript.lang.TestSpec

final class GenSpec extends TestSpec:
  "Gen" when {
    "name is generated" should {
      "do it deterministically" in {
        val g0 = Gen.empty

        val (g1, s1) = g0.name()
        g1.seed mustBe (1)
        s1 mustBe ("a")

        val (g11, s11) = g0.name()
        g11.seed mustBe (1)
        s11 mustBe ("a")

        val (g2, s2) = g1.name()
        g2.seed mustBe (2)
        s2 mustBe ("b")
      }
    }
  }
