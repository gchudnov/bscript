# BScript Toolkit

AST Evaluation & Debugging

## Modules

### Serde

* [/serde](serde) - A Serializer & Deserializer for AST.

```scala
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.serde.ASTSerde

val serde = ASTSerde.make()

// Serialize
val t = VarDecl(TypeRef("int"), "x", IntVal(0))
val errOrSer: Either[SerdeException, String] = serde.serialize(t)

// Deserialize
val input = """{"kind":"VarDecl","type":"int","name":"x","expr":{"kind":"IntVal","value":"0"}}"""
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

* [/interpreter](interpreter) - Interprets built AST.

```scala
val astMeta: AstMeta    = ???
val laws: InterpretLaws = ???

val errOrRes: Either[Throwable, Cell] = Interpreter.interpret(astMeta.ast, laws, astMeta.meta)
```

### Translator

* [/translator](translator) - Translates AST to the given programming language. At the moment only Scala is supported.

## Implementations

### B1

* [/b1](b1) - Library that implements laws for B1 Language. Depends on `serde`, `builder`, `interpreter` and `translator` modules.

### B1-CLI

* [/b1-cli](b1-cli) - Command-Line Utility for B1 Language.
