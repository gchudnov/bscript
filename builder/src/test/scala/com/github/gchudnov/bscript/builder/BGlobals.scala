package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.types.TypeName

import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import scala.util.control.Exception.allCatch

/**
 * Globals for building
 */
object BGlobals:

  def make(): AST =

    val builtInTypes = Block.of(
      Annotated.std(TypeDecl(TypeName.nothing)),
      Annotated.std(TypeDecl(TypeName.void)),
      Annotated.std(TypeDecl(TypeName.bool)),
      Annotated.std(TypeDecl(TypeName.i8)),
      Annotated.std(TypeDecl(TypeName.i16)),
      Annotated.std(TypeDecl(TypeName.i32)),
      Annotated.std(TypeDecl(TypeName.i64)),
      Annotated.std(TypeDecl(TypeName.f32)),
      Annotated.std(TypeDecl(TypeName.f64)),
      Annotated.std(TypeDecl(TypeName.dec)),
      Annotated.std(TypeDecl(TypeName.chr)),
      Annotated.std(TypeDecl(TypeName.str)),
      Annotated.std(TypeDecl(TypeName.date)),
      Annotated.std(TypeDecl(TypeName.datetime)),
      Annotated.std(StructDecl("Vec", List(TypeDecl("T")), List.empty[VarDecl])),
      Annotated.std(StructDecl("Map", List(TypeDecl("K"), TypeDecl("V")), List.empty[VarDecl])),
    )

    val builtInMethods = Block.of(
      // prints the formatted string to StdOut
      MethodDecl(
        "printf",
        List(TypeDecl("T")),
        List(
          VarDecl("format", TypeId(TypeName.str)),
          VarDecl("value", TypeId("T"))
        ),
        TypeId(TypeName.void),
        Block.empty
      ),
      // returns the length of the provided string
      MethodDecl(
        "strLen",
        List.empty[TypeDecl],
        List(
          VarDecl("s", TypeId(TypeName.str))
        ),
        TypeId(TypeName.i32),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // offsets the provided date-time
      MethodDecl(
        "offsetDateTime",
        List.empty[TypeDecl],
        List(
          VarDecl("value", TypeId(TypeName.datetime)),
          VarDecl("offset", TypeId(TypeName.i32)),
          VarDecl("unit", TypeId(TypeName.str))
        ),
        TypeId(TypeName.datetime),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // sets data and time to the specified value
      MethodDecl(
        "setDateTime",
        List.empty[TypeDecl],
        List(
          VarDecl("value", TypeId(TypeName.datetime)),
          VarDecl("offset", TypeId(TypeName.i32)),
          VarDecl("unit", TypeId(TypeName.str))
        ),
        TypeId(TypeName.datetime),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // returns the specified part of date-time as an integer value
      MethodDecl(
        "fieldOfDateTime",
        List.empty[TypeDecl],
        List(
          VarDecl("value", TypeId(TypeName.datetime)),
          VarDecl("unit", TypeId(TypeName.str))
        ),
        TypeId(TypeName.i32),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
        )
      ),
      // returns true of the provided variable is defined, otherwise false
      MethodDecl(
        "isDefined",
        List(TypeDecl("T")),
        List(
          VarDecl("x", TypeId("T"))
        ),
        TypeId(TypeName.bool),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.bool))
        )
      ),
      // returns the first non-null value out of two values that were provided
      MethodDecl(
        "coalesce",
        List(TypeDecl("T")),
        List(
          VarDecl("x", TypeId("T")),
          VarDecl("y", TypeId("T"))
        ),
        TypeId("T"),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // returns today as a date
      MethodDecl(
        "today",
        List.empty[VarDecl],
        TypeId(TypeName.date),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.date))
        )
      ),
      // returns current date and time as date-time
      MethodDecl(
        "now",
        List.empty[VarDecl],
        TypeId(TypeName.datetime),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
        )
      ),
      // rounds the provided value with the given precision
      MethodDecl(
        "round",
        List(TypeDecl("T")),
        List(
          VarDecl("value", TypeId("T")), // f32, f64, dec
          VarDecl("precision", TypeId(TypeName.i32))
        ),
        TypeId("T"),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      ),
      // truncates the provided value with the given precision
      MethodDecl(
        "truncate",
        List(TypeDecl("T")),
        List(
          VarDecl("value", TypeId("T")), // f32, f64, dec
          VarDecl("precision", TypeId(TypeName.i32))
        ),
        TypeId("T"),
        Block.of(
          Compiled(callback = Compiled.identity, retType = TypeId("T"))
        )
      )
    )

    builtInTypes ++ builtInMethods
