package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef

final class ScopeSymbolsSpec extends TestSpec:

  "ScopeSymbolsSpec" when {

    "scope is added" should {
      val a = ScopeRef("a")

      val ss = ScopeSymbols
        .empty
        .addScope(a)

      "contain the newly added scope" in {
        ss.scopeSymbols.isEmpty mustBe(false)
        ss.symbolScopes.isEmpty mustBe(true)

        ss.scopeSymbols must contain theSameElementsAs(List(ScopeRef("a") -> List.empty[Symbol]))
      }

      "new symbols can be linked to that scope" in {
        val m = SymbolRef("myFunc")

        // val ss1 = ss.link()
      }
    }
  }