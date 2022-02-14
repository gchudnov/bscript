package com.github.gchudnov.bscript.inspector

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types

final class InspectorSpec extends TestSpec:
  "Inspector" when {
    "AST is inspected" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct A {
       *       int x;
       *       B b;
       *     };
       *     struct B { int y; };
       *
       *     A a;
       *
       *     void g(int x) { a.x = x; }
       *     void f(int x) { int y = 1; g(2*x); a.b.y = 2; }
       *     void main() { f(3); }
       *
       *     main();
       *     a;
       *   }
       * }}}
       */      
      "provide information about the AST between function calls" in {
        // TODO: impl it
      }
    }
  }
