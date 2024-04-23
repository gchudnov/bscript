package com.github.gchudnov.bscript.translator.internal.c

import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.symbols.VectorType
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.translator.TGlobals
import com.github.gchudnov.bscript.translator.TTypeCheckLaws
import com.github.gchudnov.bscript.translator.TestSpec
import com.github.gchudnov.bscript.translator.internal.c.laws.CTranslateLaws
import com.github.gchudnov.bscript.translator.laws.TypeInit

import java.time.LocalDate

final class CVisitorSpec extends TestSpec:

  private val typeNames: TypeNames = TGlobals.typeNames

  "CVisitor" when {

    "unary minus" should {
      /**
       * {{{
       *  -10;
       * }}}
       */
      "translate to c" in {
        val t = UnaryMinus(IntVal(10))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual   = s.show()
            val expected = "-10"

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast0: AST): Either[Throwable, CState] =
    val types = Types.make(typeNames)
    val typeCheckLaws = TTypeCheckLaws.make(types)

    Builder
      .build(ast0, types, typeCheckLaws)
      .flatMap(astMeta =>
        val typeInit = CTypeInit
        val laws = CTranslateLaws.make(typeNames, typeInit, astMeta.meta)

        val cVisitor: CVisitor = CVisitor.make(laws)
        val cState: CState = CState.make(astMeta.meta)

        astMeta.ast.visit(cState, cVisitor)
      )
