# BScript Toolkit

AST Evaluation & Debugging

## Modules

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

// Translate AST to Scala
val errOrScala: Either[Throwable, String] = B1.translateScala(ast0)
```

### B1-CLI

* [/b1-cli](b1-cli) - Command-Line Utility for B1 Language.

Allows to run and debug AST.

```bash
# TBD
```
