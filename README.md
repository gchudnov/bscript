# BScript Toolkit

AST Evaluation and Debugging 

* [/serde](serde) - Serializer and Deserializer for AST.

```scala
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.serde.ASTSerde

val t = VarDecl(TypeRef("i32"), "x", IntVal(0))

val errOrSerialized = ASTSerde.ast.serialize(t)
```
