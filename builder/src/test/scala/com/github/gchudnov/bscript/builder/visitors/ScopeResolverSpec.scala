package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver
import com.github.gchudnov.bscript.builder.visitors.ScopeBuilder
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.const.*

final class ScopeResolverSpec extends TestSpec:

  "ScopeResolverSpec" when {
    "no input" should {
      val sb = ScopeBuilder.make().push().define(SBuiltIn.i32)
      val sr = sb.toResolver

      "return the initial object state" in {
        val actual   = sr.result
        val expected = sb.result

        actual mustBe (expected)
      }
    }

    "defined symbol" should {
      val ast1 = VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(12)))
      val ast2 = VarDecl("y", TypeId(TypeName.i32), Literal(IntVal(23)))

      val sb = ScopeBuilder.make().push().define(SBuiltIn.i32).push().bind(ast1).push().push().bind(ast2).define(SVar("y"))
      val sr = sb.toResolver.asInstanceOf[BasicScopeResolver]

      "resolve" in {
        val actual1 = sr.resolveUp(TypeName.i32, sr.scopeFor(ast1).get)
        val actual2 = sr.resolveUp(TypeName.i32, sr.scopeFor(ast2).get)

        actual1.isDefined mustBe (true)
        actual2.isDefined mustBe (true)
      }

      "resolveIn" in {
        val actual = sr.resolveIn("y", sr.scopeFor(ast2).get)

        actual.isDefined mustBe (true)
      }
    }

    "undefined symbol" should {
      val ast = VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(12)))

      val sb = ScopeBuilder.make().push().define(SBuiltIn.i32).push().bind(ast)
      val sr = sb.toResolver.asInstanceOf[BasicScopeResolver]

      "not resolve" in {
        val actual = sr.resolveUp("custom-type", sr.scopeFor(ast).get)

        actual.isDefined mustBe (false)
      }

      "not resolveIn" in {
        val actual = sr.resolveIn("custom-type", sr.scopeFor(ast).get)

        actual.isDefined mustBe (false)
      }
    }

    "scope" should {
      val ast = Literal(IntVal(12))

      val sb = ScopeBuilder.make().push().bind(ast).pop()
      val sr = sb.toResolver.asInstanceOf[BasicScopeResolver]

      "resolve if AST was bounded" in {
        val actual = sr.scopeFor(ast)

        actual.isDefined mustBe (true)
      }

      "not resolve if AST was not bounded" in {
        val ast1 = Literal(IntVal(11))

        val actual = sr.scopeFor(ast1)

        actual.isDefined mustBe (false)
      }
    }
  }
