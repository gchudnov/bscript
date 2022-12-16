package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.symbols.Symbol


final class ScopeDirectorySpec extends TestSpec:

  "ScopeDirectorySpec" when {

    "scope is added" should {
      val a = ScopeRef("a")

      val ss = ScopeDirectory.empty[Symbol]
        .addScope(a)

      "contain the newly added scope" in {
        ss.scopeEntries must contain theSameElementsAs (List(ScopeRef("a") -> Set.empty[Ptr[Symbol]]))
        ss.entryScopes must contain theSameElementsAs (List.empty)
      }

      "new symbols can be linked to that scope" in {
        val m = SymbolRef("myFunc")

        val ss1 = ss.link(m, a)

        ss1.scopeEntries must contain theSameElementsAs (List(a -> Set(Ptr(m))))
        ss1.entryScopes must contain theSameElementsAs (List(Ptr(m) -> a))
      }

      // "symbols must be unique" in {
      //   val m1 = SymbolRef("myFunc")
      // }
    }
  }
