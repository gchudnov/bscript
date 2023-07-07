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
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.refs.*
import com.github.gchudnov.bscript.lang.ast.types.StructType

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
      "resolve the type" in {
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
        
      val structTypeFinder = new ASTFinder:
        override def findAST(ast: AST): Option[AST] =
          ast match
            case _: StructType => Some(ast)
            case _             => None

      /**
       * {{{
       *   // globals
       *   {
       *     struct A { };
       *     A a;
       *     a
       *   }
       * }}}
       */
      "eval to void and type is resolved" in {
        val t = Examples.structEmpty

        val structDeclFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: StructDecl => Some(ast)
              case _             => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val declNode = structDeclFinder.foldAST(None, t.ast)
            actualState.evalTypes(declNode.get) mustBe BuiltInType(TypeName.void)

            val typeNode = structTypeFinder.foldAST(None, t.ast)
            actualState.evalTypes(typeNode.get) mustBe BuiltInType(TypeName.void)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      // TODO: fix it ^^^

      /**
       * {{{
       *   // globals
       *   {
       *     struct<T> A { };
       *   }
       * }}}
       */
      "eval to void generic structs" in {
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

      "eval type when one field" in {

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
            fail("Should be 'right", t)
      }
    }

    "id" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   x;
       * }}}
       */
      "resolve type for variable declarations without auto" in {
        val t = Examples.xDeclReturnX

        val xFinder = idFinder("x")

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = xFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   long y = 1;
       *   y;
       * }}}
       */
      "resolve type if several delarations are present" in {
        val t = Examples.xyDeclReturnY

        val yFinder = idFinder("y")

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = yFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i64)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   x;
       * }}}
       */
      "fail to resolve an Id if the var is not declared" in {
        val t = Examples.xRefUndefined

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("Id 'x' is not found")
      }

      /**
       * {{{
       *   // globals
       *   UnknownType x = 0;
       *   x;
       * }}}
       */
      "fail to resolve the TypeId if the reference doesn't exist" in {
        val t = Examples.xDeclUnknownType

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("TypeId 'UnknownType' is not found")
      }

      /**
       * {{{
       *   // globals
       *   auto x = 0; // shold be auto-deduced to type: i32
       *   x;
       * }}}
       */
      "resolve type with auto-declarations" in {
        val t = Examples.autoDeclReturnX

        val xFinder = idFinder("x")

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val node = xFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   auto x = 0;      // shold be auto-deduced to type: i32
       *   auto y = "abc";  // shold be auto-deduced to type: str
       *   x;
       *   y;
       * }}}
       */
      "resolve type with two auto-declarations" in {
        val t = Examples.autoDecl2ReturnX

        val xFinder = idFinder("x")
        val yFinder = idFinder("y")

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val xNode = xFinder.foldAST(None, t.ast)
            val yNode = yFinder.foldAST(None, t.ast)
            actualState.evalTypes(xNode.get) mustBe BuiltInType(TypeName.i32)
            actualState.evalTypes(yNode.get) mustBe BuiltInType(TypeName.str)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   long x = _;
       *   x;
       * }}}
       */
      "resolve declaration with default initialization" in {
        val t = Examples.xDeclDfaultReturnX

        val xFinder = idFinder("x")

        val initFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: Init => Some(ast)
              case _       => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val xNode    = xFinder.foldAST(None, t.ast)
            val initNode = initFinder.foldAST(None, t.ast)
            actualState.evalTypes(xNode.get) mustBe BuiltInType(TypeName.i64)
            actualState.evalTypes(initNode.get) mustBe BuiltInType(TypeName.i64)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   auto x = _;
       *   x;
       * }}}
       */
      "fail to resolve if auto x = init" in {
        val t = Examples.xDeclAutoInit

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("Auto() = Init() is not supported")
      }
    }
  }

  /**
   * To evaluate, we run Phases 1, 2 and 3.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve symbols in AST-nodes.
   *   - In Phase 3 we resolve types of AST-nodes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 scope build
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0

    val buildOut = buildPass.run(buildIn)

    // #2 symbol resolve
    val symResolvePass = new SymbolResolvePass()
    val symResolveIn = new HasReadScopeTree with HasReadScopeSymbols with HasReadScopeAsts with HasAST:
      override val scopeTree: ReadScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ReadScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts           = buildOut.scopeAsts
      override val ast: AST                       = ast0

    val _ = symResolvePass.run(symResolveIn)

    // #3 type resolve
    val typeResolvePass = new TypeResolvePass()
    val typeResolveIn = new HasReadScopeTree with HasReadScopeSymbols with HasReadScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val typeResolveOut = typeResolvePass.run(typeResolveIn)

    // return the actual state
    val actualState = toActualState(typeResolveOut)
    actualState
  }

  def idFinder(id: String): ASTFinder = new ASTFinder:
    override def findAST(ast: AST): Option[AST] =
      ast match
        case Id(name) if name == id => Some(ast)
        case _                      => None

object TypeResolvePassSpec:
  final case class ActualState(
    evalTypes: EvalTypes,
  )

  def toActualState(s: HasEvalTypes): ActualState =
    ActualState(
      evalTypes = s.evalTypes,
    )
