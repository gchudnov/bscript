package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.BuilderException

final class ScopeBuilderSpec extends TestSpec:

  "ScopeBuilderSpec" when {
    "no input was specified" should {
      "return an empty object" in {
        val sb = ScopeBuilder.make()

        val actual   = sb.result
        val expected = Meta.empty

        actual mustBe expected
      }
    }

    "no scope was pushed" should {
      "prohibit symbol definition" in {
        val sym = SymbolRef.date

        intercept[BuilderException] {
          ScopeBuilder.make().define(sym)
        }
      }
    }

    "a scope was pushed" should {
      val sb = ScopeBuilder
        .make()
        .push()

      "return state with one scope" in {
        val actual = sb.result

        val expected = Meta(
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols.empty
        )

        actual mustBe expected
      }

      "a symbol can be linked to this scope" in  {
        val sym = SymbolRef.f32

        val actual = sb.define(sym).result

        val expected = Meta(
          forest = Forest(Set(ScopeRef("a")), Map.empty[Scope, Scope]),
          scopeSymbols = ScopeSymbols(Map(ScopeRef("a") -> Set(sym)), Map(Ptr(sym) -> ScopeRef("a")))
        )

        actual mustBe expected
      }
    }
  }
