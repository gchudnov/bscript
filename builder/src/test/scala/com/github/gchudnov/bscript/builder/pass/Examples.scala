package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.util.Transform

/**
 * A named Example.
 */
final case class Example(name: String, ast: AST)

/**
 * A collection of ASTs for testing purposes.
 */
object Examples:

  /**
   * Variable Declaration
   *
   * {{{
   *   // globals
   *   int x = 0;
   * }}}
   */
  val ex1: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0)))
    )
    Example("ex1", t)

  /**
   * Function Declaration
   *
   * {{{
   *   // globals
   *   int main() {
   *     int x;
   *     x = 3;
   *   }
   * }}}
   */
  val ex2: Example =
    val t = Block.of(
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32)
        ),
        Block.of(
          VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Assign(Id("x"), ConstLit(IntVal(3)))
        )
      )
    )
    Example("ex2", t)

  /**
   * A function with one argument and name shadowing
   *
   * {{{
   *   // globals
   *   int myFunc(long x) {
   *     int x;
   *     x = 3;
   *   }
   * }}}
   */
  val ex3: Example =
    val t = Block.of(
      MethodDecl(
        "myFunc",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.i32)
        ),
        Block.of(
          VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Assign(Id("x"), ConstLit(IntVal(3)))
        )
      )
    )
    Example("ex3", t)

  /**
   * Several method declarations
   *
   * {{{
   *   // globals
   *   {
   *       datetime offsetDateTime(value: datetime, offset: int, unit: str) {
   *         // ...
   *       }
   *
   *       int fieldOfDateTime(value: datetime, unit: str) {
   *         // ...
   *       }
   *   }
   * }}}
   */
  val ex4: Example =
    val t = Block.of(
      MethodDecl(
        "offsetDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("offset", TypeId(TypeName.i32)),
            VarDecl("unit", TypeId(TypeName.str))
          ),
          TypeId(TypeName.datetime)
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      MethodDecl(
        "fieldOfDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("unit", TypeId(TypeName.str))
          ),
          TypeId(TypeName.i32)
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      )
    )
    Example("ex4", t)

  /**
   * Plus for integers
   *
   * {{{
   *   int +(lhs: int, rhs: int) {
   *     // ...
   *   }
   *
   *   2 + 3
   * }}}
   */
  val ex5: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.i32)),
            VarDecl("rhs", TypeId(TypeName.i32))
          ),
          TypeId(TypeName.i32)
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      Call(Id("+"), List(ConstLit(IntVal(2)), ConstLit(IntVal(3))))
    )
    Example("ex5", t)

  /**
   * Plus for different data types
   *
   * {{{
   *   double +(lhs: double, rhs: int) {
   *     // ...
   *   }
   *
   *   1.0 + 3
   * }}}
   */
  val ex6: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.f64)),
            VarDecl("rhs", TypeId(TypeName.i32))
          ),
          TypeId(TypeName.f64)
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.f64))
        )
      ),
      Call(Id("+"), List(ConstLit(DoubleVal(1.0)), ConstLit(IntVal(3))))
    )
    Example("ex6", t)

  /**
   * Plus on string and integer
   *
   * {{{
   *   int +(lhs: int, rhs: int) {
   *     // ...
   *   }
   *
   *   "abc" + 3  // incorrect, but it is reported later during compilation on the next phases
   * }}}
   */
  val ex7: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.i32)),
            VarDecl("rhs", TypeId(TypeName.i32))
          ),
          TypeId(TypeName.i32)
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      Call(Id("+"), List(ConstLit(StrVal("abc")), ConstLit(IntVal(3))))
    )
    Example("ex7", t)

  /**
   * Generic plus
   *
   * {{{
   *   T +[T](lhs: T, rhs: T) {
   *     // ...
   *   }
   *
   *   4 + 3
   * }}}
   */
  val ex8: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("lhs", TypeId("T")),
            VarDecl("rhs", TypeId("T"))
          ),
          TypeId("T")
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      Call(Id("+"), List(ConstLit(StrVal("abc")), ConstLit(IntVal(3))))
    )
    Example("ex8", t)
