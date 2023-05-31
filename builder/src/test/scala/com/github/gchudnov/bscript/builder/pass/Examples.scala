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
   *   fn main() -> int = {
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
   *   fn myFunc(long x) -> int {
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
   *       fn offsetDateTime(value: datetime, offset: int, unit: str) -> datetime = {
   *         // ...
   *       }
   *
   *       fn fieldOfDateTime(value: datetime, unit: str) -> int = {
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
   *   fn +(lhs: int, rhs: int) -> int {
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
   *   fn +(lhs: double, rhs: int) -> double {
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
   *   fn +(lhs: int, rhs: int) -> int {
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
   *   fn +[T](lhs: T, rhs: T) -> T {
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

  /**
   * Generic plus with multiple type parameters
   *
   * {{{
   *   fn +[R, T, U](lhs: T, rhs: U) -> R {
   *     // ...
   *   }
   *
   *   4 + 3
   * }}}
   */
  val ex9: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("R"), TypeDecl("T"), TypeDecl("U")),
          List(
            VarDecl("lhs", TypeId("T")),
            VarDecl("rhs", TypeId("U"))
          ),
          TypeId("R")
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("R"))
        )
      ),
      Call(Id("+"), List(ConstLit(IntVal(4)), ConstLit(IntVal(3))))
    )
    Example("ex9", t)

  /**
   * Several variables in a scope
   *
   * {{{
   *   // globals
   *   {
   *     int i = 9;
   *     float j;
   *     int k = i+2;
   *   }
   * }}}
   */
  val ex10: Example =
    val t = Block.of(
      VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(9))),
      VarDecl("j", TypeId(TypeName.f32), ConstLit(FloatVal(0.0f))),
      VarDecl("k", TypeId(TypeName.i32), Call(Id("+"), List(Id("i"), ConstLit(IntVal(2)))))
    )
    Example("ex10", t)

  /**
   * Collection literal where the type is defined automatically
   *
   * {{{
   *   // globals
   *   {
   *     auto a = [1, 2, 3];
   *   }
   * }}}
   */
  val ex11: Example =
    val t = Block.of(
      VarDecl("a", Auto(), CollectionLit(VecType(Auto()), List(ConstLit(IntVal(1)), ConstLit(IntVal(2)), ConstLit(IntVal(3)))))
    )
    Example("ex11", t)

  /**
   * Collection literal where the type is passed manually
   *
   * {{{
   *   // globals
   *   {
   *     int[] a = [1, 2, 3];
   *   }
   * }}}
   */
  val ex12: Example =
    val t = Block.of(
      VarDecl("a", VecType(TypeId(TypeName.i32)), CollectionLit(Auto(), List(ConstLit(IntVal(1)), ConstLit(IntVal(2)), ConstLit(IntVal(3)))))
    )
    Example("ex12", t)

  /**
   * Declare variable and assign it to Null; auto-type deduction
   *
   * {{{
   *   // globals
   *   {
   *     auto x = nothing;
   *   }
   * }}}
   */
  val ex13: Example =
    val t = Block.of(
      VarDecl("x", Auto(), ConstLit(NullVal()))
    )
    Example("ex13", t)

  /**
   * Declare variable and assign it to Null; manual type specification
   *
   * {{{
   *   // globals
   *   {
   *     int x = nothing;
   *   }
   * }}}
   */
  val ex14: Example =
    val t = Block.of(
      VarDecl("x", TypeId("i32"), ConstLit(NullVal()))
    )
    Example("ex14", t)

  /**
   * Several scopes, variable use from the non-available scope
   *
   * {{{
   *   // globals
   *   {
   *     fn printf(format: str, value: auto) -> void = { }
   *
   *     { int z = 0; }       // local scope nested within f's local scope
   *     printf("%d", z); // z is no longer visible; static analysis ERROR!
   *   }
   * }}}
   */
  val ex15: Example =
    val t = Block.of(
      MethodDecl(
        "printf",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("format", TypeId(TypeName.str)),
            VarDecl("value", Auto())
          ),
          TypeId(TypeName.void)
        ),
        Block.empty
      ),
      Block.of(
        VarDecl("z", TypeId(TypeName.i32), ConstLit(IntVal(0)))
      ),
      Call(Id("printf"), List(ConstLit(StrVal("%d")), Id("z"))) // z is no longer visible; Will be an error in Phase #2
    )
    Example("ex15", t)

  /**
   * Several nested scopes
   *
   * {{{
   * // globals
   *   {
   *     int x;                // define variable x in global scope
   *     fn f() -> void = {    // define function f in global scope
   *       int y;              // define variable y in local scope of f
   *       { int i; }          // define variable i in nested local scope
   *       { int j; }          // define variable j in another nested local scope
   *     }
   *     fn g() -> void = {    // define function g in global scope
   *       int i;              // define variable i in local scope of g
   *     }
   *   }
   * }}}
   */
  val ex16: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      MethodDecl(
        "f",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void)
        ),
        Block.of(
          VarDecl("y", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Block.of(
            VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(0)))
          ),
          Block.of(
            VarDecl("j", TypeId(TypeName.i32), ConstLit(IntVal(0)))
          )
        )
      ),
      MethodDecl(
        "g",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void)
        ),
        Block.of(
          VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(0)))
        )
      )
    )
    Example("ex16", t)

  /**
   * Structure definition and usage
   *
   * {{{
   *   // globals
   *   {
   *     struct B { int y; };
   *     struct C { int z; };
   *     struct A {
   *       int x;
   *       B b;
   *       C c;
   *     };
   *
   *     A a;
   *
   *     fn f() -> void = {
   *       struct D {
   *         int i;
   *       };
   *
   *       D d;
   *       d.i = a.b.y;
   *     }
   *   }
   * }}}
   */
  val ex17: Example =
    val t = Block.of(
      StructDecl("B", StructType(List.empty[TypeDecl], List(VarDecl("y", TypeId(TypeName.i32))))),
      StructDecl("C", StructType(List.empty[TypeDecl], List(VarDecl("z", TypeId(TypeName.i32))))),
      StructDecl("A", StructType(List.empty[TypeDecl], List(VarDecl("x", TypeId(TypeName.i32)), VarDecl("b", TypeId("B")), VarDecl("c", TypeId("C"))))),
      VarDecl("a", TypeId("A")),
      MethodDecl(
        "f",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void)
        ),
        Block.of(
          StructDecl("D", StructType(List.empty[TypeDecl], List(VarDecl("i", TypeId(TypeName.i32))))),
          VarDecl("d", TypeId("D")),
          Assign(
            Access(Id("d"), Id("i")),
            Access(Access(Id("a"), Id("b")), Id("y"))
          )
        )
      )
    )
    Example("ex17", t)

  /**
   * Struct initalization
   *
   * {{{
   *   // globals
   *   {
   *     struct B { int y; };
   *
   *     struct A {
   *       int x;
   *       string s;
   *       B b;
   *     };
   *
   *     A a = { 1, "hello", { 2 } };
   *     a
   *   }
   * }}}
   */
  val ex18: Example =
    val t = Block.of(
      StructDecl("B", StructType(List.empty[TypeDecl], List(VarDecl("y", TypeId(TypeName.i32))))),
      StructDecl("A", StructType(List.empty[TypeDecl], List(VarDecl("x", TypeId(TypeName.i32)), VarDecl("s", TypeId(TypeName.str)), VarDecl("b", TypeId("B"))))),
      VarDecl(
        "a",
        TypeId("A"),
        CollectionLit(
          TypeId("A"),
          List(
            KeyValue(ConstLit(StrVal("x")), ConstLit(IntVal(1))),
            KeyValue(ConstLit(StrVal("s")), ConstLit(StrVal("alice"))),
            KeyValue(
              ConstLit(StrVal("b")),
              CollectionLit(
                TypeId("B"),
                List(KeyValue(ConstLit(StrVal("y")), ConstLit(IntVal(2))))
              )
            )
          )
        )
      ),
      Id("a")
    )
    Example("ex18", t)

  /**
   * A small program
   * {{{
   *   // globals
   *   {
   *     int x = 1;
   *     fn g(int x) -> void = { int z = 2; }
   *     fn f(int x) -> void = { int y = 1; g(2*x); }
   *     fn main() -> void = { f(3); }
   *     main();
   *   }
   * }}}
   */
  val ex19: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(1))),
      MethodDecl(
        "g",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.void)
        ),
        Block.of(
          VarDecl("z", TypeId(TypeName.i32), ConstLit(IntVal(2)))
        )
      ),
      MethodDecl(
        "f",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.void)
        ),
        Block.of(
          VarDecl("y", TypeId(TypeName.i32), ConstLit(IntVal(1))),
          Call(Id("g"), List(Call(Id("*"), List(ConstLit(IntVal(2)), Id("x")))))
        )
      ),
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void)
        ),
        Block.of(
          Call(Id("f"), List(ConstLit(IntVal(3))))
        )
      ),
      Call(Id("main"), List.empty[Expr])
    )
    Example("ex19", t)

  /**
   * {{{
   *   // given a collection of words, create a map of word lengths
   *   def main(): map[str, i32] = {
   *     auto as = ["alice", "bob", "carol"];
   *
   *     val f = [](as: []str) -> map[str, i32] {
   *       val m = map[string, int]{};
   *
   *       val iterate = [](i: i32) -> map[str, i32] {
   *         if len(m) == i {
   *           m;
   *         } else {
   *           w = get(as, i)
   *           m = set(m, w, len(w))
   *           iterate(i + 1)
   *         }
   *       }
   *
   *       iterate(0);
   *     };
   *
   *     f(as); // returns a map of lengths
   *   }
   *
   *   main();
   * }}}
   */
  val ex20: Example =
    val t = Block.empty // TODO: implement
    Example("ex20", t)
