package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.internal.MetaOps.findMember
import com.github.gchudnov.bscript.lang.ast.{ ArgDecl, Block as AstBlock, MethodDecl }
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.util.EqWrap

final class ScopeStateSpec extends TestSpec:

  "ScopeState" when {
    "define" should {
      "define a standalone (root) scope" in {
        val b0 = SBlock("b0")

        val t1 = Meta.empty.defineBlock(b0)

        t1.scopeTree.vertices.size mustBe (1)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0))
        t1.scopeTree.edges.size mustBe (0)
      }

      "define a Block" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineBlock(b1, b0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1))
        t1.scopeTree.edges.size mustBe (1)
        t1.scopeTree.parent(b1) mustBe (Some(b0))
      }

      "define a Method" in {
        val b0 = SBlock("b0")
        val m0 = SMethod("main")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineMethod(m0, b0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(m0))
        t1.scopeTree.edges.size mustBe (1)
        t1.scopeTree.parent(m0) mustBe (Some(b0))

        t1.scopeSymbols.size mustBe (1)
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(m0)))

        t1.symbolScopes.size mustBe (1)
        t1.symbolScopes.get(EqWrap(m0)) mustBe (Some(b0))

        t1.methodAsts.size mustBe (0)
      }

      "define a Struct" in {
        val b0 = SBlock("b0")
        val k0 = SStruct("struct")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineStruct(k0, b0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(k0))
        t1.scopeTree.edges.size mustBe (1)
        t1.scopeTree.parent(k0) mustBe (Some(b0))

        t1.scopeSymbols.size mustBe (1)
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(k0)))

        t1.symbolScopes.size mustBe (1)
        t1.symbolScopes.get(EqWrap(k0)) mustBe (Some(b0))

        t1.methodAsts.size mustBe (0)
      }

      "define a BuiltInType in a Block" in {
        val b0 = SBlock("b0")
        val i0 = SBuiltInType("int")
        val i1 = SBuiltInType("long")
        val i2 = SBuiltInType("double")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineBuiltInType(i0, b0)
          .defineBuiltInType(i1, b0)
          .defineBuiltInType(i2, b0)

        t1.scopeTree.vertices.size mustBe (1)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0))
        t1.scopeTree.edges.size mustBe (0)

        t1.scopeSymbols.size mustBe (1)
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(i0, i1, i2)))

        t1.symbolScopes.size mustBe (3)
        t1.symbolScopes.get(EqWrap(i0)) mustBe (Some(b0))
        t1.symbolScopes.get(EqWrap(i1)) mustBe (Some(b0))
        t1.symbolScopes.get(EqWrap(i2)) mustBe (Some(b0))
      }

      "define field in a Struct" in {
        val b0 = SBlock("b0")
        val k0 = SStruct("struct")
        val f0 = SVar("field0")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineStruct(k0, b0)
          .defineStructField(k0, f0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(k0))
        t1.scopeTree.edges.size mustBe (1)

        t1.scopeSymbols.size mustBe (2)
        t1.scopeSymbols.get(EqWrap(k0)) mustBe (Some(Set(f0)))
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(k0)))

        t1.symbolScopes.size mustBe (2)
        t1.symbolScopes.get(EqWrap(f0)) mustBe (Some(k0))
        t1.symbolScopes.get(EqWrap(k0)) mustBe (Some(b0))
      }

      "define a method argument" in {
        val b0 = SBlock("b0")
        val m0 = SMethod("main")
        val a0 = SVar("arg0")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineMethod(m0, b0)
          .defineMethodArg(m0, a0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(m0))
        t1.scopeTree.edges.size mustBe (1)

        t1.scopeSymbols.size mustBe (2)
        t1.scopeSymbols.get(EqWrap(m0)) mustBe (Some(Set(a0)))
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(m0)))

        t1.symbolScopes.size mustBe (2)
        t1.symbolScopes.get(EqWrap(a0)) mustBe (Some(m0))
        t1.symbolScopes.get(EqWrap(m0)) mustBe (Some(b0))

        t1.methodArgs.size mustBe (1)
        t1.methodArgs.get(EqWrap(m0)) mustBe (Some(List(a0)))
      }

      "define the method return type" in {
        val b0 = SBlock("b0")
        val i0 = SBuiltInType("int")
        val m0 = SMethod("main")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineBuiltInType(i0, b0)
          .defineMethod(m0, b0)
          .defineMethodRetType(m0, i0)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(m0))
        t1.scopeTree.edges.size mustBe (1)

        t1.scopeSymbols.size mustBe (1)
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(m0, i0)))

        t1.symbolScopes.size mustBe (2)
        t1.symbolScopes.get(EqWrap(i0)) mustBe (Some(b0))
        t1.symbolScopes.get(EqWrap(m0)) mustBe (Some(b0))

        t1.methodRetTypes.size mustBe (1)
        t1.methodRetTypes.get(EqWrap(m0)) mustBe (Some(i0))
      }

      "define the method AST" in {
        val b0 = SBlock("b0")
        val m0 = SMethod("main")

        val astM = MethodDecl(TypeRef("void"), "printf", List.empty[ArgDecl], AstBlock.empty)

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineMethod(m0, b0)
          .defineMethodAST(m0, astM)

        t1.scopeTree.vertices.size mustBe (2)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(m0))
        t1.scopeTree.edges.size mustBe (1)

        t1.methodAsts.size mustBe (1)
        t1.methodAsts.get(EqWrap(m0)) mustBe (Some(astM))
      }

      "define variable in a block" in {
        val b0 = SBlock("b0")
        val v0 = SVar("y")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineVar(v0, b0)

        t1.scopeTree.vertices.size mustBe (1)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0))
        t1.scopeTree.edges.size mustBe (0)

        t1.symbolScopes.size mustBe (1)
        t1.symbolScopes.get(EqWrap(v0)) mustBe (Some(b0))
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(v0)))
      }

      "define variable type" in {
        val b0 = SBlock("b0")
        val v0 = SVar("y")
        val p0 = TypeRef("int")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineVar(v0, b0)
          .defineVarType(v0, p0)

        t1.scopeTree.vertices.size mustBe (1)
        t1.scopeTree.vertices must contain allElementsOf List(EqWrap(b0))
        t1.scopeTree.edges.size mustBe (0)

        t1.symbolScopes.size mustBe (1)
        t1.symbolScopes.get(EqWrap(v0)) mustBe (Some(b0))
        t1.scopeSymbols.get(EqWrap(b0)) mustBe (Some(Set(v0)))

        t1.varTypes.size mustBe (1)
        t1.varTypes.get(EqWrap(v0)) mustBe (Some(p0))
      }

      "resolve a member in a scope (no parent lookup)" in {
        val b0 = SBlock("b0")
        val m0 = SMethod("main")

        val a0 = SVar("arg0")
        val a1 = SVar("arg1")
        val a2 = SVar("arg2")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineMethod(m0, b0)
          .defineMethodArg(m0, a0)
          .defineMethodArg(m0, a1)
          .defineMethodArg(m0, a2)

        findMember(t1, "arg1", m0) mustBe Some(a1)
        findMember(t1, "arg1", b0) mustBe None
      }

      /**
       * {{{
       *   x |     b0
       *           /  \
       *   y |   b1  b2
       *               \
       *               b3  | z
       * }}}
       */
      "resolve a symbol recursively" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")
        val b3 = SBlock("b3")

        val x = SVar("x")
        val y = SVar("y")
        val z = SVar("z")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineBlock(b1, b0)
          .defineBlock(b2, b0)
          .defineBlock(b3, b2)
          .defineVar(x, b0)
          .defineVar(y, b1)
          .defineVar(z, b3)

        // x
        t1.resolve("x", b0) mustBe Right(x)
        t1.resolve("x", b1) mustBe Right(x)
        t1.resolve("x", b2) mustBe Right(x)
        t1.resolve("x", b3) mustBe Right(x)

        // y
        t1.resolve("y", b0).isLeft mustBe true
        t1.resolve("y", b1) mustBe Right(y)
        t1.resolve("y", b2).isLeft mustBe true
        t1.resolve("y", b3).isLeft mustBe true

        // z
        t1.resolve("z", b0).isLeft mustBe true
        t1.resolve("z", b1).isLeft mustBe true
        t1.resolve("z", b2).isLeft mustBe true
        t1.resolve("z", b3) mustBe Right(z)
      }
    }

    "show" should {
      import Meta.{ *, given }

      "display an empty state" in {
        val t1 = Meta.empty

        val s = t1.show

        s mustBe ("""{
                     |  "scopeTree": {
                     |    "vertices": [],
                     |    "edges": []
                     |  },
                     |  "scopeSymbols": {
                     |  },
                     |  "symbolScopes": {
                     |  },
                     |  "methodArgs": {
                     |  },
                     |  "methodRetTypes": {
                     |  },
                     |  "methodAsts": {
                     |  },
                     |  "varTypes": {
                     |  },
                     |  "methods": [
                     |  ]
                     |}
                     |""".stripMargin.trim)
      }

      "display a non-empty state" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")
        val b3 = SBlock("b3")

        val m0 = SMethod("main")

        val a0 = SVar("arg0")
        val a1 = SVar("arg1")
        val a2 = SVar("arg2")

        val p0 = TypeRef("int")
        val p1 = TypeRef("float")
        val p2 = TypeRef("decimal")

        val astM = MethodDecl(TypeRef("void"), "printf", List.empty[ArgDecl], AstBlock.empty)

        val x = SVar("x")
        val y = SVar("y")
        val z = SVar("z")

        val t1 = Meta.empty
          .defineBlock(b0)
          .defineBlock(b1, b0)
          .defineBlock(b2, b0)
          .defineBlock(b3, b2)
          .defineVar(x, b0)
          .defineVar(y, b1)
          .defineVar(z, b3)
          .defineMethod(m0, b0)
          .defineMethodArg(m0, a0)
          .defineMethodArg(m0, a1)
          .defineMethodArg(m0, a2)
          .defineMethodRetType(m0, p0)
          .defineMethodAST(m0, astM)
          .defineVarType(a0, p0)
          .defineVarType(a1, p1)
          .defineVarType(a2, p2)
          .defineVarType(x, p0)
          .defineVarType(y, p1)
          .defineVarType(z, p2)

        val s = t1.show

        s mustBe ("""{
                     |  "scopeTree": {
                     |    "vertices": ["b0","b1","b2","b3","main"],
                     |    "edges": [["b1","b0"],["b2","b0"],["b3","b2"],["main","b0"]]
                     |  },
                     |  "scopeSymbols": {
                     |    "b0": ["main","x"],
                     |    "b1": ["y"],
                     |    "b3": ["z"],
                     |    "main": ["arg0","arg1","arg2"]
                     |  },
                     |  "symbolScopes": {
                     |    "arg0": "main",
                     |    "arg1": "main",
                     |    "arg2": "main",
                     |    "main": "b0",
                     |    "x": "b0",
                     |    "y": "b1",
                     |    "z": "b3"
                     |  },
                     |  "methodArgs": {
                     |    "main": ["arg0","arg1","arg2"]
                     |  },
                     |  "methodRetTypes": {
                     |    "main": "int"
                     |  },
                     |  "methodAsts": {
                     |    "main": "(omitted)"
                     |  },
                     |  "varTypes": {
                     |    "arg0": "int",
                     |    "arg1": "float",
                     |    "arg2": "decimal",
                     |    "x": "int",
                     |    "y": "float",
                     |    "z": "decimal"
                     |  },
                     |  "methods": [
                     |    "fn main(arg0: int, arg1: float, arg2: decimal) -> int"
                     |  ]
                     |}
                     |""".stripMargin.trim)
      }
    }
  }
