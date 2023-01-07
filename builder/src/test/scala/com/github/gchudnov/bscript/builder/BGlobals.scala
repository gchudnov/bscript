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
      TypeDecl(TypeName.nothing),
      TypeDecl(TypeName.void),
      TypeDecl(TypeName.bool),
      TypeDecl(TypeName.i8),
      TypeDecl(TypeName.i16),
      TypeDecl(TypeName.i32),
      TypeDecl(TypeName.i64),
      TypeDecl(TypeName.f32),
      TypeDecl(TypeName.f64),
      TypeDecl(TypeName.dec),
      TypeDecl(TypeName.chr),
      TypeDecl(TypeName.str),
      TypeDecl(TypeName.date),
      TypeDecl(TypeName.datetime)
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
