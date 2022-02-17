package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.rewriter.internal.RTypeNames
import com.github.gchudnov.bscript.rewriter.TestSpec

final class FindVisitorSpec extends TestSpec:
  "FindVisitor" when {
    "an existing node looked up" should {
      "return it" in {
        // TODO: impl it
      }
    }

    "a missing node is looked up" should {
      "return None" in {
        // TODO: impl it
      }
    }
  }
