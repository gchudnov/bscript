package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.interpreter.memory.Cell

import java.time.LocalDate
import java.time.OffsetDateTime

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

    "difference is calculated" should {
      "calc it for primitives" in {
        val a = Cell(1)
        val b = Cell(2)

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Updated("A", a, b))

        actual.toList must contain theSameElementsAs expected
      }

      "calc it for structs" in {
        val cellA1 = Cell(1)
        val cellA2 = Cell(2)
        val cellB  = Cell("alice")
        val cellC  = Cell(12.34)

        val a = StructCell(Map("a" -> cellA1, "b" -> cellB))
        val b = StructCell(Map("a" -> cellA2, "c" -> cellC))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Updated("A.a", cellA1, cellA2), Diff.Removed("A.b", cellB), Diff.Added("A.c", cellC))

        actual.toList must contain theSameElementsAs expected
      }

      "calc it for arrays" in {
        val a = Cell(List(Cell(1), Cell(2), Cell(3)))
        val b = Cell(List(Cell(1), Cell(2), Cell(3), Cell(4)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Added("A.2", Cell(4)))
      }

      "calc it for nested structs" in {
        val a = StructCell(Map("a" -> Cell(1), "b" -> StructCell(Map("c" -> Cell(true))), "d" -> VecCell(List(Cell("alice")))))
        val b = StructCell(Map("a" -> Cell(1), "b" -> StructCell(Map("c" -> Cell(false))), "d" -> VecCell(List(Cell("bob")))))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Updated("A.b.c", Cell(true), Cell(false)),
          Diff.Updated("A.d.0", Cell("alice"), Cell("bob"))
        )

        actual.toList must contain theSameElementsAs expected
      }
    }

    "converted to a string" should {
      import Cell.{ *, given }

      "show nothing" in {
        val cell: Cell = NothingCell
        val actual     = cell.show
        val expected   = """"nothing""""
        actual mustBe expected
      }

      "show void" in {
        val cell: Cell = VoidCell
        val actual     = cell.show
        val expected   = """"void""""
        actual mustBe expected
      }

      "show bool" in {
        val cell: Cell = BoolCell(true)
        val actual     = cell.show
        val expected   = """"bool(true)""""
        actual mustBe expected
      }

      "show i32" in {
        val cell: Cell = IntCell(123)
        val actual     = cell.show
        val expected   = """"i32(123)""""
        actual mustBe expected
      }

      "show i64" in {
        val cell: Cell = LongCell(456L)
        val actual     = cell.show
        val expected   = """"i64(456)""""
        actual mustBe expected
      }

      "show f32" in {
        val cell: Cell = FloatCell(12.34f)
        val actual     = cell.show
        val expected   = """"f32(12.34)""""
        actual mustBe expected
      }

      "show f64" in {
        val cell: Cell = DoubleCell(56.78)
        val actual     = cell.show
        val expected   = """"f64(56.78)""""
        actual mustBe expected
      }

      "show dec" in {
        val cell: Cell = DecimalCell(BigDecimal(1000.28))
        val actual     = cell.show
        val expected   = """"dec(1000.28)""""
        actual mustBe expected
      }

      "show str" in {
        val cell: Cell = StrCell("alice")
        val actual     = cell.show
        val expected   = """"str(alice)""""
        actual mustBe expected
      }

      "show date" in {
        val cell: Cell = DateCell(LocalDate.parse("2021-10-04"))
        val actual     = cell.show
        val expected   = """"date(2021-10-04)""""
        actual mustBe expected
      }

      "show datetime" in {
        val cell: Cell = DateTimeCell(OffsetDateTime.parse("2021-10-04T01:00:00+02:00"))
        val actual     = cell.show
        val expected   = """"datetime(2021-10-04T01:00+02:00)""""
        actual mustBe expected
      }

      "show vec of primitives" in {
        val cell: Cell = VecCell(List[Cell](IntCell(1), LongCell(1000L), StrCell("alice")))
        val actual     = cell.show
        val expected = """[
                         |  "i32(1)", 
                         |  "i64(1000)", 
                         |  "str(alice)"
                         |]""".stripMargin
        actual mustBe expected
      }

      "show struct" in {
        val cell: Cell = StructCell(Map("a" -> IntCell(1), "b" -> StrCell("alice")))
        val actual     = cell.show
        val expected = s"""{
                          |  "a": "i32(1)",
                          |  "b": "str(alice)"
                          |}""".stripMargin
        actual mustBe expected
      }

      "show nested struct" in {
        val cell: Cell = StructCell(Map("a" -> IntCell(1), "b" -> StructCell(Map("c" -> StrCell("alice")))))
        val actual     = cell.show
        val expected = s"""{
                          |  "a": "i32(1)",
                          |  "b": {
                          |    "c": "str(alice)"
                          |  }
                          |}""".stripMargin
        actual mustBe expected
      }

      "show vec of structs" in {
        val cell: Cell = VecCell(
          List[Cell](
            StructCell(Map("a" -> IntCell(1), "b" -> StrCell("alice"))),
            StructCell(Map("a" -> IntCell(2), "b" -> StrCell("bob")))
          )
        )
        val actual = cell.show
        val expected = s"""[
                          |  {
                          |    "a": "i32(1)",
                          |    "b": "str(alice)"
                          |  }, 
                          |  {
                          |    "a": "i32(2)",
                          |    "b": "str(bob)"
                          |  }
                          |]""".stripMargin
        actual mustBe expected
      }
    }
  }
