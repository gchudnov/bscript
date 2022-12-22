package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.visitors.ScopeBuilder
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.const.*

final class ScopeResolverSpec extends TestSpec:

  "ScopeResolverSpec" when {
    "no input" should {
      "return the initial object state" in {
        val sb = ScopeBuilder.make().push().define(SBuiltIn.i32)
        val sr = sb.toResolver

        val actual = sr.result
        val expected = sb.result

        actual mustBe(expected)
      }
    }

    "defined symbol" should {
      "be resolved" in {
        val ast = VarDecl(TypeRef.i32, "x", Literal(IntVal(12)))

        val sb = ScopeBuilder.make().push().define(SBuiltIn.i32).push().bind(ast)
        val sr = sb.toResolver

        val actual = sr.resolve(SymbolRef(TypeName.i32), sr.scopeFor(ast).get)

        actual.isDefined mustBe(true)
      }
    }

    "undefined symbol" should {
      "not be resolved" in {
        val ast = VarDecl(TypeRef.i32, "x", Literal(IntVal(12)))

        val sb = ScopeBuilder.make().push().define(SBuiltIn.i32).push().bind(ast)
        val sr = sb.toResolver

        val actual = sr.resolve(SymbolRef("custom-type"), sr.scopeFor(ast).get)

        actual.isDefined mustBe(false)
      }
    }
  }