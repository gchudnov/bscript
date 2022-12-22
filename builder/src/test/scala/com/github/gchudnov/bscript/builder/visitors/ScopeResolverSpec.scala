package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.visitors.ScopeBuilder
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.*

final class ScopeResolverSpec extends TestSpec:

  "ScopeResolverSpec" when {
    "no input" should {
      "return the initial object state" in {
        val sb = ScopeBuilder.make()
        val sb1 = sb.push().define(SBuiltIn.i32)

        val sr = sb1.toResolver

        val actual = sr.result
        val expected = sb1.result

        actual mustBe(expected)
      }
    }
  }