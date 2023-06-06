package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.pass.scopebuild.PassState
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeRef
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.Tree
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.SymbolRef

final class PassStateSpec extends TestSpec:

  private val ast0    = Block.empty
  private val stateIn = InState.from(ast0)

  "BuildPassState" when {
    "no input was specified" should {
      "return an empty object" in {
        val sb = PassState.from(stateIn)

        val actual   = sb
        val expected = PassState.empty

        actual mustBe expected
      }
    }

    "no scope was pushed" should {
      "prohibit symbol definition" in {
        val sym = SymbolRef.date

        intercept[BuilderException] {
          PassState.from(stateIn).define(sym)
        }
      }
    }

    "a scope was pushed" should {
      val sb = PassState
        .from(stateIn)
        .push()

      "return state with one scope" in {
        val actual = PassState.into(sb, ast0)

        actual.scopeTree.vertexSize mustBe 1
        actual.scopeTree.edgeSize mustBe 0
      }

      "a symbol can be linked to this scope" in {
        val sym = SymbolRef.f32

        val actual = PassState.into(sb.define(sym), ast0)

        actual.scopeTree.vertexSize mustBe 1
        actual.scopeTree.edgeSize mustBe 0

        actual.scopeSymbols.symbolsByName(sym.name) mustBe List(sym)
      }
    }
  }
