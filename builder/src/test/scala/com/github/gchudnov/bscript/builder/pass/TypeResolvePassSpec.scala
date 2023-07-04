package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

import scala.util.control.Exception.*
import com.github.gchudnov.bscript.builder.Examples
import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.ast.types.BuiltInType
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.func.ASTFinder
import com.github.gchudnov.bscript.lang.ast.lit.ConstLit
import com.github.gchudnov.bscript.lang.ast.decls.VarDecl
import com.github.gchudnov.bscript.lang.ast.decls.BuiltInDecl
import com.github.gchudnov.bscript.lang.ast.decls.MethodDecl
import com.github.gchudnov.bscript.lang.ast.decls.StructDecl
import com.github.gchudnov.bscript.lang.ast.decls.TypeDecl

/**
 * Type Resolve Pass Tests
 */
final class TypeResolvePassSpec extends TestSpec:
  import TypeResolvePassSpec.*
  import TypeAST.given

  val showTypeAST = summon[Show[TypeAST]]

  "TypeResolvePass" when {

    "const literals" should {

      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "assing type" in {
        val t = Examples.intVal

        val constFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: ConstLit => Some(ast)
              case _           => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = constFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "variable declaration" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "resolve to void" in {
        val t = Examples.varDef

        val varDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: VarDecl => Some(ast)
              case _          => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = varDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "built-in declarations" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "resolve to void" in {
        val t = Examples.varDef

        val builtInDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: BuiltInDecl => Some(ast)
              case _              => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = builtInDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "method declarations" should {

      /**
       * {{{
       *   // globals
       *   fn main() -> int = {
       *     3;
       *   }
       * }}}
       */
      "resolve to void" in {
        val t = Examples.fnDecl3

        val fnDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: MethodDecl => Some(ast)
              case _             => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = fnDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "struct declarations" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct A { };
       *   }
       * }}}
       */
      "eval to void" in {
        val t = Examples.structEmpty

        val structDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: StructDecl => Some(ast)
              case _             => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = structDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "generic paramters nodes" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct<T> A { };
       *   }
       * }}}
       */
      "eval to void" in {
        val t = Examples.structT

        val typeDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: TypeDecl => Some(ast)
              case _           => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = typeDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "block takes he type of the last expression" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "in var decl" in {
        val t = Examples.varDef

        val blockFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: Block => Some(ast)
              case _        => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = blockFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "condition" should {
      val ifFinder = new ASTFinder:
        override def findAST(ast: AST): Option[AST] =
          ast match
            case _: If => Some(ast)
            case _     => None

      /**
       * {{{
       *   // globals
       *   {
       *     if(true) {
       *       4;
       *     } else {
       *       9;
       *     }
       *   }
       * }}}
       */
      "deduce type if THEN and ELSE have the same types" in {
        val t = Examples.ifThenXElseX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = ifFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     if(true) {
       *       4;
       *     } else {
       *       "alice";
       *     }
       *   }
       * }}}
       *
       * NOTE: not we return an error, but we could return a union type.
       */
      "raise an error if THEN and ELSE have different types" in {
        val t = Examples.ifThenXElseY

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("Type mismatch in if expression")
      }

      "deduce type as ELSE-type if THEN-type is nothing" in {
        val t = Examples.ifThenNothingElseY

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = ifFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.str)

          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }

      "deduce type as THEN-type if ELSE-type is nothing" in {
        val t = Examples.ifThenXElseNothing

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = ifFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phases 1, 2.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve types of AST-nodes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 build
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0

    val buildOut = buildPass.run(buildIn)

    // #2 var resolve
    val varResolvePass = new VarResolvePass()
    val varResolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val _ = varResolvePass.run(varResolveIn)

    // #3 resolve
    val typeResolvePass = new TypeResolvePass()
    val typeResolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val resolveOut = typeResolvePass.run(typeResolveIn)

    // return the actual state
    val actualState = toActualState(resolveOut)
    actualState
  }

object TypeResolvePassSpec:
  final case class ActualState(
    evalTypes: EvalTypes,
  )

  def toActualState(s: HasEvalTypes): ActualState =
    ActualState(
      evalTypes = s.evalTypes,
    )
