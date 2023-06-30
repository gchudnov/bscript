
package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.*

import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import scala.util.control.Exception.allCatch

/**
 * Globals for building
 */
object BGlobals:

  def make(): AST =

    val builtInTypes = Block.of(
      // TODO: probably we need to define Std struct and self-annotate it.
      BuiltInDecl(TypeName.nothing),
      BuiltInDecl(TypeName.void),
      BuiltInDecl(TypeName.bool),
      BuiltInDecl(TypeName.u8),
      BuiltInDecl(TypeName.i16),
      BuiltInDecl(TypeName.i32),
      BuiltInDecl(TypeName.i64),
      BuiltInDecl(TypeName.f32),
      BuiltInDecl(TypeName.f64),
      BuiltInDecl(TypeName.dec),
      BuiltInDecl(TypeName.chr),
      BuiltInDecl(TypeName.str),
      BuiltInDecl(TypeName.date),
      BuiltInDecl(TypeName.datetime),
    )

    val builtInMethods = Block.of(
      // prints the formatted string to StdOut
      MethodDecl(
        "printf",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("format", TypeId(TypeName.str)),
            VarDecl("value", TypeId("T"))
          ),
          TypeId(TypeName.void),
        ),
        Block.empty
      ),
      // returns the length of the provided string
      MethodDecl(
        "strLen",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("s", TypeId(TypeName.str))
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // offsets the provided date-time
      MethodDecl(
        "offsetDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("offset", TypeId(TypeName.i32)),
            VarDecl("unit", TypeId(TypeName.str))
          ),
          TypeId(TypeName.datetime),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // sets data and time to the specified value
      MethodDecl(
        "setDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("offset", TypeId(TypeName.i32)),
            VarDecl("unit", TypeId(TypeName.str))
          ),
          TypeId(TypeName.datetime),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // returns the specified part of date-time as an integer value
      MethodDecl(
        "fieldOfDateTime",
        MethodType(
          List.empty[TypeDecl],
          List(
            VarDecl("value", TypeId(TypeName.datetime)),
            VarDecl("unit", TypeId(TypeName.str))
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // returns true of the provided variable is defined, otherwise false
      MethodDecl(
        "isDefined",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("x", TypeId("T"))
          ),
          TypeId(TypeName.bool),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.bool))
        )
      ),
      // returns the first non-null value out of two values that were provided
      MethodDecl(
        "coalesce",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("x", TypeId("T")),
            VarDecl("y", TypeId("T"))
          ),
          TypeId("T"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // returns today as a date
      MethodDecl(
        "today",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.date),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.date))
        )
      ),
      // returns current date and time as date-time
      MethodDecl(
        "now",
        MethodType(
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.datetime),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // rounds the provided value with the given precision
      MethodDecl(
        "round",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("value", TypeId("T")), // f32, f64, dec
            VarDecl("precision", TypeId(TypeName.i32))
          ),
          TypeId("T"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // truncates the provided value with the given precision
      MethodDecl(
        "truncate",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("value", TypeId("T")), // f32, f64, dec
            VarDecl("precision", TypeId(TypeName.i32))
          ),
          TypeId("T"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // checks whether the provided array contains the given element
      MethodDecl(
        "contains",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("xs", VecType(TypeId("T"))), // array[T]
            VarDecl("x", TypeId("T")), // f32, f64, dec
          ),
          TypeId(TypeName.bool),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.bool))
        )
      ),
      // checks whether the provided map contains the given element
      MethodDecl(
        "contains",
        MethodType(
          List(TypeDecl("K"), TypeDecl("V")),
          List(
            VarDecl("x", TypeId("K")), // f32, f64, dec
            VarDecl("m", MapType(TypeId("K"), TypeId("V"))), // map[K, V]
          ),
          TypeId(TypeName.bool),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.bool))
        )
      ),
      // returns the length of the array
      MethodDecl(
        "len",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("xs", VecType(TypeId("T"))), // array[T]
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // returns the number of keys in the map
      MethodDecl(
        "len",
        MethodType(
          List(TypeDecl("K"), TypeDecl("V")),
          List(
            VarDecl("m", MapType(TypeId("K"), TypeId("V"))), // map[K, V]
          ),
          TypeId(TypeName.i32),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // gets an element of the array at position N
      MethodDecl(
        "get",
        MethodType(
          List(TypeDecl("T")),
          List(
            VarDecl("xs", VecType(TypeId("T"))), // array[T]
            VarDecl("pos", TypeId(TypeName.i32)),
          ),
          TypeId("T"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // gets an element of the map by key K
      MethodDecl(
        "get",
        MethodType(
          List(TypeDecl("K"), TypeDecl("V")),
          List(
            VarDecl("m", MapType(TypeId("K"), TypeId("V"))), // map[K, V]
            VarDecl("key", TypeId("K"))
          ),
          TypeId("V"),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("V"))
        )
      ),
      // sets the element of the map by key K
      MethodDecl(
        "set",
        MethodType(
          List(TypeDecl("K"), TypeDecl("V")),
          List(
            VarDecl("m", MapType(TypeId("K"), TypeId("V"))), // map[K, V]
            VarDecl("key", TypeId("K")),
            VarDecl("value", TypeId("V"))
          ),
           MapType(TypeId("K"), TypeId("V")),
        ),
        Block.of(
          Compiled(callback = Compiled.identity, retType =  MapType(TypeId("K"), TypeId("V")))
        )
      ),
    )

    builtInTypes ++ builtInMethods
