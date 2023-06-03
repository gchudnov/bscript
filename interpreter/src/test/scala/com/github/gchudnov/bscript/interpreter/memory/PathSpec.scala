package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.builder.TestSpec

final class PathSpec extends TestSpec {

  "Path" when {
    "parsed" should {
      "create a new path from a non-empty string" in {
        val p = Path.parse("a.b.c")
        p mustBe Path(List("a", "b", "c"))
      }

      "create an empty path from an empty string" in {
        val p = Path.parse("")
        p mustBe Path(List.empty[String])
      }
    }
  }

}