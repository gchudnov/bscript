package com.github.gchudnov.bscript.b1.internal.stdlib.io

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.b1.internal.util.DirOps
import com.github.gchudnov.bscript.b1.internal.util.FileOps
import com.github.gchudnov.bscript.b1.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.builder.AstMeta
import com.github.gchudnov.bscript.interpreter.memory.StrCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.DeclType
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef

import java.nio.file.Path
import java.nio.file.Paths
import scala.util.control.Exception.allCatch

final class ReadFileSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "ReadFile" when {
    "file is read in AST" should {
      "capture the contents" in {
        val data = "Hello World"
        val errOrRes = for
          dir     <- DirOps.createTempDir("readFile")
          filePath = dir.resolve("data.txt")
          _       <- FileOps.saveString(filePath, data)
          ast0 = Block(
                   VarDecl(TypeRef(typeNames.strType), "x", Call(SymbolRef("readFile"), List(StrVal(filePath.toString)))),
                   Var(SymbolRef("x"))
                 )
          res <- B1.run(ast0)
        yield res

        errOrRes match
          case Right(cell) =>
            cell mustBe StrCell(data)
          case Left(t) =>
            fail("Should be 'right", t)

      }

      "return an error if file is not found" in {
        // TODO: impl it
      }
    }
  }
