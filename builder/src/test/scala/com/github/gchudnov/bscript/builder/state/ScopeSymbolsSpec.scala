package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.util.Ptr

// TODO: RENAME TO ScopeDirectorySpec refactor tests

final class ScopeSymbolsSpec extends TestSpec:

  "ScopeSymbolsSpec" when {

    "scope is added" should {
      val a = ScopeRef("a")

      val ss = ScopeSymbols.empty
        .addScope(a)

      "contain the newly added scope" in {
        ss.scopeSymbols must contain theSameElementsAs (List(ScopeRef("a") -> Set.empty[Symbol]))
        ss.symbolScopes must contain theSameElementsAs (List.empty)
      }

      "new symbols can be linked to that scope" in {
        val m = SymbolRef("myFunc")

        val ss1 = ss.link(m, a)

        ss1.scopeSymbols must contain theSameElementsAs (List(a -> Set(m)))
        ss1.symbolScopes must contain theSameElementsAs (List(Ptr(m) -> a))
      }
    }
  }
