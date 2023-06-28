package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.lang.types.TypeName

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
  val exVarDef: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
    )
    Example("exVarDef", t)

  /**
   * Variable Declaration & Use
   *
   * {{{
   *   // globals
   *   int x = 0;
   *   x = 1;
   * }}}
   */
  val exVarDefUse: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      Assign(Id("x"), ConstLit(IntVal(1))),
    )
    Example("exVarDefUse", t)

  /**
   * Variable Declaration & Use another one
   * 
   * Fail since `y` is not defined
   *
   * {{{
   *   // globals
   *   int x = 0;
   *   y = 1;
   * }}}
   */
  val exVarNotDefined: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      Assign(Id("y"), ConstLit(IntVal(1))),
    )
    Example("exVarNotDefined", t)

  /**
   * Variable Declaration
   * 
   * should fail because of duplicate variable names
   *
   * {{{
   *   // globals
   *   int x = 0;
   *   int x = 1;
   * }}}
   */
  val exDoubleDef: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(1))),
    )
    Example("exDoubleDef", t)


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
  val exFnDecl: Example =
    val t = Block.of(
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
        ),
        Block.of(
          VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Assign(Id("x"), ConstLit(IntVal(3))),
        ),
      ),
    )
    Example("exFnDecl", t)

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
  val exFnVarArgShadow: Example =
    val t = Block.of(
      MethodDecl(
        "myFunc",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.i32),
        ),
        Block.of(
          VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Assign(Id("x"), ConstLit(IntVal(3))),
        ),
      ),
    )
    Example("exFnVarArgShadow", t)

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
  val exTwoMethods: Example =
    val t = Block.of(
      MethodDecl(
        "offsetDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("offset", TypeId(TypeName.i32)),
            VarDecl("unit", TypeId(TypeName.str)),
          ),
          TypeId(TypeName.datetime),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime)),
        ),
      ),
      MethodDecl(
        "fieldOfDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("unit", TypeId(TypeName.str)),
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32)),
        ),
      ),
    )
    Example("exTwoMethods", t)

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
  val exPlusInt: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.i32)),
            VarDecl("rhs", TypeId(TypeName.i32)),
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32)),
        ),
      ),
      Call(Id("+"), List(ConstLit(IntVal(2)), ConstLit(IntVal(3)))),
    )
    Example("exPlusInt", t)

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
  val exPlusDoubleInt: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.f64)),
            VarDecl("rhs", TypeId(TypeName.i32)),
          ),
          TypeId(TypeName.f64),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.f64)),
        ),
      ),
      Call(Id("+"), List(ConstLit(DoubleVal(1.0)), ConstLit(IntVal(3)))),
    )
    Example("exPlusDoubleInt", t)

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
  val exPlusStringInt: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("lhs", TypeId(TypeName.i32)),
            VarDecl("rhs", TypeId(TypeName.i32)),
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32)),
        ),
      ),
      Call(Id("+"), List(ConstLit(StrVal("abc")), ConstLit(IntVal(3)))),
    )
    Example("exPlusStringInt", t)

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
  val exPlusGeneric: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("lhs", TypeId("T")),
            VarDecl("rhs", TypeId("T")),
          ),
          TypeId("T"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T")),
        ),
      ),
      Call(Id("+"), List(ConstLit(StrVal("abc")), ConstLit(IntVal(3)))),
    )
    Example("exPlusGeneric", t)

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
  val exPlusT: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("R"), TypeDecl("T"), TypeDecl("U")),
          List(
            VarDecl("lhs", TypeId("T")),
            VarDecl("rhs", TypeId("U")),
          ),
          TypeId("R"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("R")),
        ),
      ),
      Call(Id("+"), List(ConstLit(IntVal(4)), ConstLit(IntVal(3)))),
    )
    Example("exPlusT", t)

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
  val exMultipleVars: Example =
    val t = Block.of(
      VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(9))),
      VarDecl("j", TypeId(TypeName.f32), ConstLit(FloatVal(0.0f))),
      VarDecl("k", TypeId(TypeName.i32), Call(Id("+"), List(Id("i"), ConstLit(IntVal(2))))),
    )
    Example("exMultipleVars", t)

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
  val exAutoCol: Example =
    val t = Block.of(
      VarDecl("a", Auto(), CollectionLit(VecType(Auto()), List(ConstLit(IntVal(1)), ConstLit(IntVal(2)), ConstLit(IntVal(3))))),
    )
    Example("exAutoCol", t)

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
  val exVarCol: Example =
    val t = Block.of(
      VarDecl("a", VecType(TypeId(TypeName.i32)), CollectionLit(Auto(), List(ConstLit(IntVal(1)), ConstLit(IntVal(2)), ConstLit(IntVal(3))))),
    )
    Example("exVarCol", t)

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
  val exAutoNothing: Example =
    val t = Block.of(
      VarDecl("x", Auto(), ConstLit(NullVal)),
    )
    Example("exAutoNothing", t)

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
  val exIntNothing: Example =
    val t = Block.of(
      VarDecl("x", TypeId("i32"), ConstLit(NullVal)),
    )
    Example("exIntNothing", t)

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
  val exVarNotInScope: Example =
    val t = Block.of(
      MethodDecl(
        "printf",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("format", TypeId(TypeName.str)),
            VarDecl("value", Auto()),
          ),
          TypeId(TypeName.void),
        ),
        Block.empty,
      ),
      Block.of(
        VarDecl("z", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      ),
      Call(Id("printf"), List(ConstLit(StrVal("%d")), Id("z"))), // z is no longer visible; Will be an error in Phase #2
    )
    Example("exVarNotInScope", t)

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
  val exNestedScopes: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(0))),
      MethodDecl(
        "f",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void),
        ),
        Block.of(
          VarDecl("y", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          Block.of(
            VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          ),
          Block.of(
            VarDecl("j", TypeId(TypeName.i32), ConstLit(IntVal(0))),
          ),
        ),
      ),
      MethodDecl(
        "g",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void),
        ),
        Block.of(
          VarDecl("i", TypeId(TypeName.i32), ConstLit(IntVal(0))),
        ),
      ),
    )
    Example("exNestedScopes", t)

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
  val exStruct: Example =
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
          TypeId(TypeName.void),
        ),
        Block.of(
          StructDecl("D", StructType(List.empty[TypeDecl], List(VarDecl("i", TypeId(TypeName.i32))))),
          VarDecl("d", TypeId("D")),
          Assign(
            Access(Id("d"), Id("i")),
            Access(Access(Id("a"), Id("b")), Id("y")),
          ),
        ),
      ),
    )
    Example("exStruct", t)

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
  val exStructInit: Example =
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
                List(KeyValue(ConstLit(StrVal("y")), ConstLit(IntVal(2)))),
              ),
            ),
          ),
        ),
      ),
      Id("a"),
    )
    Example("exStructInit", t)

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
  val exSmallApp: Example =
    val t = Block.of(
      VarDecl("x", TypeId(TypeName.i32), ConstLit(IntVal(1))),
      MethodDecl(
        "g",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.void),
        ),
        Block.of(
          VarDecl("z", TypeId(TypeName.i32), ConstLit(IntVal(2))),
        ),
      ),
      MethodDecl(
        "f",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.void),
        ),
        Block.of(
          VarDecl("y", TypeId(TypeName.i32), ConstLit(IntVal(1))),
          Call(Id("g"), List(Call(Id("*"), List(ConstLit(IntVal(2)), Id("x"))))),
        ),
      ),
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.void),
        ),
        Block.of(
          Call(Id("f"), List(ConstLit(IntVal(3)))),
        ),
      ),
      Call(Id("main"), List.empty[Expr]),
    )
    Example("exSmallApp", t)

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
  val exAdvApp: Example =
    val t = Block.empty // TODO: implement
    Example("exAdvApp", t)

  /**
   * Const literal -- Integer
   * {{{
   *   // globals
   *   2;
   * }}}
   */
  val exInt: Example =
    val t = Block.of(
      ConstLit(IntVal(2)),
    )
    Example("exInt", t)

  /**
   * Const literal -- Integers in a block
   * {{{
   *   // globals
   *   2
   *   3
   * }}}
   */
  val exTwoInts: Example =
    val t = Block.of(
      ConstLit(IntVal(2)),
      ConstLit(IntVal(3)),
    )
    Example("exTwoInts", t)

  /**
   * Const literal -- nested block
   * {{{
   *   // globals
   *   2
   *   {
   *     3
   *   }
   * }}}
   */
  val exBlockNested: Example =
    val t = Block.of(
      ConstLit(IntVal(2)),
      Block.of(
        ConstLit(IntVal(3)),
      ),
    )
    Example("exBlockNested", t)

  /**
   * Const literal -- last value is not in a block
   * {{{
   *   // globals
   *   2
   *   {
   *     3
   *   }
   *   4
   * }}}
   */
  val exBlockInner: Example =
    val t = Block.of(
      ConstLit(IntVal(2)),
      Block.of(
        ConstLit(IntVal(3)),
      ),
      ConstLit(IntVal(4)),
    )
    Example("exBlockInner", t)

  /**
   * Method Declaration
   * 
   * should fail because of duplicate method signatutes
   *
   * {{{
   *   // globals
   *   fn main() -> int = {
   *     0;
   *   }
   * 
   *   fn main() -> int = {
   *     1;
   *   }
   * }}}
   */
  val exDefMethodSameSig: Example =
    val t = Block.of(
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
        ),
        Block.of(
          ConstLit(IntVal(0)),
        ),
      ),
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
        ),
        Block.of(
          ConstLit(IntVal(1)),
        ),
      ),
    )
    Example("exDefMethodSameSig", t)

  /**
   * Method Declaration
   * 
   * should allow different method signatutes that have the same name
   *
   * {{{
   *   // globals
   *   fn main(x: int) -> int = {
   *     0;
   *   }
   * 
   *   fn main() -> int = {
   *     1;
   *   }
   * }}}
   */
  val exDefMethodDiffSig: Example =
    val t = Block.of(
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32))),
          TypeId(TypeName.i32),
        ),
        Block.of(
          ConstLit(IntVal(0)),
        ),
      ),
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
        ),
        Block.of(
          ConstLit(IntVal(1)),
        ),
      ),
    )
    Example("exDefMethodDiffSig", t)

  /**
   * Method Declaration
   * 
   * raise an error if two function definitions with the same name different only in the return type
   *
   * {{{
   *   // globals
   *   fn main() -> long = {
   *     0L;
   *   }
   * 
   *   fn main() -> int = {
   *     1;
   *   }
   * }}}
   */
  val exDefMethodDiffRetType: Example =
    val t = Block.of(
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i64),
        ),
        Block.of(
          ConstLit(LongVal(0)),
        ),
      ),
      MethodDecl(
        "main",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
        ),
        Block.of(
          ConstLit(IntVal(1)),
        ),
      ),
    )
    Example("exDefMethodDiffSig", t)

  /**
   * Generic methods, parameters have different names
   * 
   * should produce an error
   *
   * {{{
   *   fn +[R, T, U](lhs: T, rhs: U) -> R {
   *     // ...
   *   }
   * 
   *   fn +[X, Y, Z](lhs: Y, rhs: Z) -> X {
   *     // ...
   *   }
   * }}}
   */
  val exTwoSameGenericMethods: Example =
    val t = Block.of(
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("R"), TypeDecl("T"), TypeDecl("U")),
          List(
            VarDecl("lhs", TypeId("T")),
            VarDecl("rhs", TypeId("U")),
          ),
          TypeId("R"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("R")),
        ),
      ),
      MethodDecl(
        "+",
        MethodType(
          List(TypeDecl("X"), TypeDecl("Y"), TypeDecl("Z")),
          List(
            VarDecl("lhs", TypeId("Y")),
            VarDecl("rhs", TypeId("Z")),
          ),
          TypeId("X"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("X")),
        ),
      ),
    )
    Example("exTwoSameGenericMethods", t)
