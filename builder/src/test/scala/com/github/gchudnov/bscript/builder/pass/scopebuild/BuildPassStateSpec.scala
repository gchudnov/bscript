package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.pass.scopebuild.PassState
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.SymbolRef

final class PassStateSpec extends TestSpec:

  private val ast0 = Block.empty
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

        val expected = OutState(
          ast = ast0,
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols.empty,
          scopeAsts = ScopeAsts.empty
        )

        actual mustBe expected
      }

      "a symbol can be linked to this scope" in {
        val sym = SymbolRef.f32

        val actual = PassState.into(sb.define(sym), ast0)

        val expected = OutState(
          ast = ast0,
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols(Map(ScopeRef("a") -> Set(Ptr(sym))), Map(Ptr(sym) -> ScopeRef("a"))),
          scopeAsts = ScopeAsts.empty
        )

        actual mustBe expected
      }
    }
  }
