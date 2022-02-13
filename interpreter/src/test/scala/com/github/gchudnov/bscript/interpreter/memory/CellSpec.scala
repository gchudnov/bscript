package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec

final class CellSpec extends TestSpec:
  "Cell" when {
    "two cells are merged" should {
      "perform it for structs" in {
        val a = StructCell(
          Map(
            "a" -> Cell(1),
            "b" -> Cell("alice"),
            "c" -> Cell(true),
            "d" -> StructCell(Map("e" -> Cell(3.14)))
          )
        )

        val b = StructCell(
          Map(
            "a" -> Cell(2),
            "c" -> Cell(false),
            "d" -> StructCell(Map("e" -> Cell(6.28)))
          )
        )

        val actual = Cell.merge(a, b)

        val expected = StructCell(
          Map(
            "a" -> Cell(2),
            "b" -> Cell("alice"),
            "c" -> Cell(false),
            "d" -> StructCell(Map("e" -> Cell(6.28)))
          )
        )

        actual mustBe Right(expected)
      }
    }
  }
