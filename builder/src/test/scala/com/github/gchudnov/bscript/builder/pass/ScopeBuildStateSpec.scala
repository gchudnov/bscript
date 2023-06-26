package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.SType

/**
  * Scope Build State Tests
  */
final class ScopeBuildStateSpec extends TestSpec:

  private val state0 = ScopeBuildState.empty

  "ScopeBuildState" when {

    "no scope was pushed" should {
      "prohibit symbol definition" in {
        val sym = SType.bool

        assertThrows[BuilderException] {
          state0.defineSymbol(sym)
        }
      }
    }

    "a scope was pushed" should {
      val state1 = state0.pushScope()

      "return state with one scope" in {
        state1.scopeTree.vertexSize mustBe 1
        state1.scopeTree.edgeSize mustBe 0
      }

      "a symbol can be linked to this scope" in {
        val sym = SType.f32

        val state2 = state1.defineSymbol(sym)

        state2.scopeTree.vertexSize mustBe 1
        state2.scopeTree.edgeSize mustBe 0

        state2.scopeSymbols.symbols mustBe List(sym)
      }
    }
  }
