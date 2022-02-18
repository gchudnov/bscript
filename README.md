# BScript Toolkit

AST Evaluation & Debugging

## Modules

### Lang

* [/lang](lang) - Basic Language primitives - AST, Symbols & Types.

`TreeVisitor[S, R]` - trait that all AST-processors must implement.

Supported nodes:

```scala
Init
UnaryMinus
Add
Sub
Mul
Div
Mod
Less
LessEqual
Greater
GreaterEqual
Equal
NotEqual
Not
And
Or
Assign
NothingVal
VoidVal
BoolVal
IntVal
LongVal
FloatVal
DoubleVal
DecimalVal
StrVal
DateVal
DateTimeVal
StructVal
Vec
Var
ArgDecl
VarDecl
FieldDecl
MethodDecl
StructDecl
Block
Call
If
Access
CompiledExpr
```

### Rewriter

* [/rewriter](rewriter) - Rewrites AST

* `FilterVisitor` - filter AST nodes.
* `MapVisitor` - map AST nodes.
* `FindVisitor` - find AST nodes.

```scala
val ast0: AST = ???

// filter
val pred: (AST) => Boolean = ???
val errOrRes: Either[Throwable, Option[AST]] = Rewriter.filter(ast0, pred)

// map
val f: (AST) => AST = ???
val errOrRes: Either[Throwable, AST] = Rewriter.map(ast0, f)

// find
val pred: (AST) => Boolean = ???
val errOrRes: Either[Throwable, Option[AST]] = Rewriter.find(ast0, pred)
```

### Serde

* [/serde](serde) - A Serializer & Deserializer for AST.

```scala
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.serde.JSONSerde

val serde = JSONSerde.make()

// Serialize
val t = VarDecl(TypeRef("i32"), "x", IntVal(0))
val errOrSer: Either[SerdeException, String] = serde.serialize(t)

// Deserialize
val input = """{"kind":"VarDecl","type":"i32","name":"x","expr":{"kind":"IntVal","value":"0"}}"""
val errORDes: Either[SerdeException: AST] = serde.deserialize(input)
```

NOTE: Serialization is performed without symbols, in this way if deserialized back, AST should be built again before evaluation.

NOTE: nodes that contain `StdAnn` annotation are omitted from serialization.

### Builder

* [/builder](builder) - Processes AST to define symbols, resolve scopes and assign types. After building, AST can be interpreted.

```scala
val typeNames: TypeNames         = ???
val types: Types                 = Types.make(typeNames)
val typeCheckLaws: TypeCheckLaws = ???

val ast0 = Block(
  VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
  VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
  Var(SymbolRef("y"))
)

val errOrRes: Either[Throwable, AstMeta] = Builder.build(ast0, types, typeCheckLaws)
```

### Interpreter

* [/interpreter](interpreter) - Interprets AST that has been built.

```scala
val astMeta: AstMeta             = ???
val interpretLaws: InterpretLaws = ???

val errOrRes: Either[Throwable, Cell] = Interpreter.interpret(astMeta.ast, astMeta.meta, interpretLaws)
```

### Translator

* [/translator](translator) - Translates AST to the given programming language. At the moment only translation to Scala is supported.

```scala
val ast1: AST              = ???
val typeNames: TypeNames   = ???
val meta: Meta             = ???

val errOrRes: Either[Throwable, String] = Translator.translateScala(ast1, typeNames, meta)
```

### Inspector

* [/inspector](inspector) - updates AST to trace memory changes between function calls.

```scala
val ast0: AST              = ???
val typeNames: TypeNames   = ???

val errOrRes: Either[Throwable, AST] = Inspector.memWatch("a.b", ast0, typeNames)
```

### B1

* [/b1](b1) - Library that implements laws for B1 Language. Depends on `serde`, `builder`, `interpreter` and `translator` modules.

Allows to load & save AST, build and interpret it, translate it to Scala.

```scala
val str = """{"kind":"VarDecl","type":"i32","name":"x","expr":{"kind":"IntVal","value":"0"}}"""

// Load AST from JSON
val errOrAst: Either[Throwable, AST] = B1.load(str)

// Save AST to JSON
val ast0: AST = ???
val errOrStr: Either[Throwable, String] = B1.save(ast0)

// Build AST
val errOrAstMeta: Either[Throwable, AstMeta] = B1.build(ast0)

// Interpret AST
val astMeta: AstMeta = ???    // AST that was built before with Metadata
val errOrCell: Either[Throwable, Cell] = B1.interpret(astMeta)

// Run - A shortcut for `B1.build` and `B1.interpret`
val errOrCell: Either[Throwable, Cell] = B1.run(ast0)

// Debug - Augment code with memory tracing and Run it
val errOrRes: Either[Throwable, (Cell, Seq[MemWatchDiff])] = B1.debug("a", ast0)

// Translate AST to Scala
val errOrScala: Either[Throwable, String] = B1.translateScala(ast0)
```

### B1-CLI

* [/b1-cli](b1-cli) - Command-Line Utility for B1 Language.

Allows to run and debug AST.

Assembly `b1-cli`first.

```bash
sbt assembly
```

Run:

```bash
cd ./target

# run
./b1-cli ../examples/ast-example-1.json

# debug
./b1-cli --debug --cell="a" ../examples/ast-example-1.json
./b1-cli --debug --cell="a" ../examples/ast-example-2.json
./b1-cli --debug --cell="y" ../examples/ast-example-3.json
```
