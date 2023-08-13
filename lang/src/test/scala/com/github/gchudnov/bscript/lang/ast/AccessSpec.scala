package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.TestSpec
import org.scalatest.prop.TableDrivenPropertyChecks.*
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id

final class AccessSpec extends TestSpec:

  "Access" when {
    "access is provided" should {
      "convert to a path" in {
        val t = Table(
          ("input", "expected"),
          (Access(Id("x"), Id("y")), List("x", "y")),
          (Access(Access(Id("a"), Id("b")), Id("y")), List("a", "b", "y"))
        )

        forAll(t) { (input, expected) =>
          val actual = input.path
          assert(actual == expected)
        }
      }
    }
  }
