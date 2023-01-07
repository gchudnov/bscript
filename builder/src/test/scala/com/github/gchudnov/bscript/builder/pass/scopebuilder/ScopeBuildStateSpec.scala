package com.github.gchudnov.bscript.builder.pass.scopebuilder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.state.ScopeAsts

import com.github.gchudnov.bscript.builder.pass.scopebuilder.ScopeBuildState

final class ScopeBuildStateSpec extends TestSpec:

  private val ast = Block.empty
  private val in = ScopeBuildInState(ast)

  "ScopeBuildState" when {
    "no input was specified" should {
      "return an empty object" in {
        val sb = ScopeBuildState.from(in)

        val actual   = sb
        val expected = ScopeBuildState.empty

        actual mustBe expected
      }
    }

    "no scope was pushed" should {
      "prohibit symbol definition" in {
        val sym = SymbolRef.date

        intercept[BuilderException] {
          ScopeBuildState.from(in).define(sym)
        }
      }
    }

    "a scope was pushed" should {
      val sb = ScopeBuildState
        .from(in)
        .push()

      "return state with one scope" in {
        val actual = ScopeBuildState.to(ast, sb)

        val expected = ScopeBuildOutState(
          ast = ast,
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols.empty,
          scopeAsts = ScopeAsts.empty
        )

        actual mustBe expected
      }

      "a symbol can be linked to this scope" in  {
        val sym = SymbolRef.f32

        val actual = ScopeBuildState.to(ast, sb.define(sym))

        val expected = ScopeBuildOutState(
          ast = ast,
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols(Map(ScopeRef("a") -> Set(Ptr(sym))), Map(Ptr(sym) -> ScopeRef("a"))),
          scopeAsts = ScopeAsts.empty
        )

        actual mustBe expected
      }
    }
  }
