# BScript Toolkit

AST Evaluation & Debugging

* [/serde](serde) - Serializer and Deserializer for AST.

```scala
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.serde.ASTSerde

val t = VarDecl(TypeRef("i32"), "x", IntVal(0))

val errOrSer: Either[SerdeException, String] = ASTSerde.ast.serialize(t)
```

NOTE: Serialization is performed without symbols, in this way if deserialized back, AST should be built again before evaluation.
