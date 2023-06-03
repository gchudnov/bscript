package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.interpreter.memory.Cell

import java.time.LocalDate
import java.time.OffsetDateTime

final class CellSpec extends TestSpec:
  "Cell" when {
    "merge" should {

      /**
        * {{{
        * strict a {
        *   a: 1,
        *   b: "alice",
        *   c: true,
        *   d: {
        *     e: 3.14
        * }
        * 
        * struct b {
        *   a: 2,
        *   c: false,
        *   d: {
        *     e: 6.28
        *   }
        * }
        * 
        * merged:

        * struct c {
        *   a: 2,
        *   b: "alice",
        *   c: false,
        *   d: {
        *     e: 6.28
        *   }
        * } 
        * 
        * }}}
        */
      "merge two structs" in {
        val a = Cell.Struct(
          Map(
            "a" -> Cell.I32(1),
            "b" -> Cell.Str("alice"),
            "c" -> Cell.Bool(true),
            "d" -> Cell.Struct(Map("e" -> Cell.F64(3.14)))
          )
        )

        val b = Cell.Struct(
          Map(
            "a" -> Cell.I32(2),
            "c" -> Cell.Bool(false),
            "d" -> Cell.Struct(Map("e" -> Cell.F64(6.28)))
          )
        )

        val actual = Cell.merge(a, b)

        val expected = Cell.Struct(
          Map(
            "a" -> Cell.I32(2),
            "b" -> Cell.Str("alice"),
            "c" -> Cell.Bool(false),
            "d" -> Cell.Struct(Map("e" -> Cell.F64(6.28)))
          )
        )

        actual mustBe Right(expected)
      }
    }

    "difference" should {

      /**
        * {{{
        *   a = 1
        *   b = 2
        * 
        *   diff(a, b) = [
        *     Updated("A", 1, 2)
        *   ]
        * }}}
        */
      "diff for primitives" in {
        val a = Cell.I32(1)
        val b = Cell.I32(2)

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Updated(Path.parse("A"), a, b))

        actual.toList must contain theSameElementsAs expected
      }

      /**
       * {{{
       * struct a = {
       *   a: 1,
       *   b: "alice"
       * }
       * 
       * struct b = {
       *   a: 2,
       *   c: 12.34
       * }
       * 
       * diff(a, b) = [
       *   Updated("A.a", 1, 2),
       *   Removed("A.b", "alice"),
       *   Added("A.c", 12.34)
       * ]
       * }}}
       */
      "diff for structs" in {
        val cellA1 = Cell.I32(1)
        val cellA2 = Cell.I32(2)
        val cellB  = Cell.Str("alice")
        val cellC  = Cell.F64(12.34)

        val a = Cell.Struct(Map("a" -> cellA1, "b" -> cellB))
        val b = Cell.Struct(Map("a" -> cellA2, "c" -> cellC))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Updated(Path.parse("A.a"), cellA1, cellA2), 
          Diff.Removed(Path.parse("A.b"), cellB), 
          Diff.Added(Path.parse("A.c"), cellC),
        )

        actual.toList must contain theSameElementsAs expected
      }

      /**
        * {{{
        * a = [1, 2, 3]
        * b = [1, 2, 3, 4]
        * 
        * diff(a, b) = [
        *   Added("A.3", 4)
        * ]
        * }}}
        */
      "diff for arrays if len(a) < len(b)" in {
        val a = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3)))
        val b = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3), Cell.I32(4)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Added(Path.parse("A.3"), Cell.I32(4)))

        actual mustBe(expected)
      }

      /**
        * {{{
        * a = [1, 2, 3, 4]
        * b = [1, 2, 3]
        * 
        * diff(a, b) = [
        *   Removed("A.3", 4)
        * ]
        * }}}
        */
      "diff for arrays if len(a) > len(b)" in {
        val a = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3), Cell.I32(4)))
        val b = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(Diff.Removed(Path(List("A", "3")), Cell.I32(4)))

        actual mustBe(expected)
      }

      /**
        * {{{
        * a = [1, 2, 3]
        * b = [1, 2, 3]
        * 
        * diff(a, b) = [
        * ]
        * }}}
        */
      "diff for arrays if a == b" in {
        val a = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3)))
        val b = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List.empty[Diff.Change[Path, Cell]]

        actual mustBe(expected)
      }

      /**
        * {{{
        * a = []
        * b = [1, 2, 3, 4]
        * 
        * diff(a, b) = [
        *   Added("A.0", 1),
        *   Added("A.1", 2),
        *   Added("A.2", 3),
        *   Added("A.3", 4)
        * ]
        * }}}
        */
      "diff for arrays if a.isEmpty && b.nonEmpty" in {
        val a = Cell.Vec(List.empty[Cell])
        val b = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3), Cell.I32(4)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Added(Path(List("A", "0")), Cell.I32(1)), 
          Diff.Added(Path(List("A", "1")), Cell.I32(2)), 
          Diff.Added(Path(List("A", "2")), Cell.I32(3)), 
          Diff.Added(Path(List("A", "3")), Cell.I32(4))
        )

        actual mustBe(expected)
      }

      /**
        * {{{
        * a = [1, 2, 3, 4]
        * b = []
        * 
        * diff(a, b) = [
        *   Removed("A.0", 1),
        *   Removed("A.1", 2),
        *   Removed("A.2", 3),
        *   Removed("A.3", 4)
        * ]
        * }}}
        */
      "diff for arrays if a.nonEmpty && b.isEmpty" in {
        val a = Cell.Vec(List(Cell.I32(1), Cell.I32(2), Cell.I32(3), Cell.I32(4)))
        val b = Cell.Vec(List.empty[Cell])

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Removed(Path(List("A", "0")), Cell.I32(1)), 
          Diff.Removed(Path(List("A", "1")), Cell.I32(2)), 
          Diff.Removed(Path(List("A", "2")), Cell.I32(3)), 
          Diff.Removed(Path(List("A", "3")), Cell.I32(4))
        )

        actual mustBe(expected)
      }

      /**
        * {{{
        * a = [1, 2]
        * b = [3, 4]
        * 
        * diff(a, b) = [
        *   Updated("A.0", 1, 3),
        *   Updated("A.1", 2, 4)
        * ]
        * }}}
        */
      "diff for arrays if len(a) == len(b)" in {
        val a = Cell.Vec(List(Cell.I32(1), Cell.I32(2)))
        val b = Cell.Vec(List(Cell.I32(3), Cell.I32(4)))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Updated(Path(List("A", "0")), Cell.I32(1), Cell.I32(3)), 
          Diff.Updated(Path(List("A", "1")), Cell.I32(2), Cell.I32(4))
        )

        actual mustBe(expected)
      }

      "diff for nested structs" in {
        val a = Cell.Struct(Map("a" -> Cell.I32(1), "b" -> Cell.Struct(Map("c" -> Cell.Bool(true))), "d" -> Cell.Vec(List(Cell.Str("alice")))))
        val b = Cell.Struct(Map("a" -> Cell.I32(1), "b" -> Cell.Struct(Map("c" -> Cell.Bool(false))), "d" -> Cell.Vec(List(Cell.Str("bob")))))

        val actual = Cell.diff("A", Some(a), Some(b))

        val expected = List(
          Diff.Updated("A.b.c", Cell.Bool(true), Cell.Bool(false)),
          Diff.Updated("A.d.0", Cell.Str("alice"), Cell.Str("bob"))
        )

        actual.toList must contain theSameElementsAs expected
      }
    }

    "converted to a string" should {
      import Cell.{ *, given }

      "show nothing" in {
        val cell: Cell = Cell.Nothing
        val actual     = cell.show
        val expected   = """"nothing""""
        actual mustBe expected
      }

      "show void" in {
        val cell: Cell = Cell.Void
        val actual     = cell.show
        val expected   = """"void""""
        actual mustBe expected
      }

      "show bool" in {
        val cell: Cell = Cell.Bool(true)
        val actual     = cell.show
        val expected   = """"bool(true)""""
        actual mustBe expected
      }

      "show i32" in {
        val cell: Cell = Cell.I32(123)
        val actual     = cell.show
        val expected   = """"i32(123)""""
        actual mustBe expected
      }

      "show i64" in {
        val cell: Cell = Cell.I64(456L)
        val actual     = cell.show
        val expected   = """"i64(456)""""
        actual mustBe expected
      }

      "show f32" in {
        val cell: Cell = Cell.F32(12.34f)
        val actual     = cell.show
        val expected   = """"f32(12.34)""""
        actual mustBe expected
      }

      "show f64" in {
        val cell: Cell = Cell.F64(56.78)
        val actual     = cell.show
        val expected   = """"f64(56.78)""""
        actual mustBe expected
      }

      "show dec" in {
        val cell: Cell = Cell.Dec(BigDecimal(1000.28))
        val actual     = cell.show
        val expected   = """"dec(1000.28)""""
        actual mustBe expected
      }

      "show str" in {
        val cell: Cell = Cell.Str("alice")
        val actual     = cell.show
        val expected   = """"str(alice)""""
        actual mustBe expected
      }

      "show date" in {
        val cell: Cell = Cell.Date(LocalDate.parse("2021-10-04"))
        val actual     = cell.show
        val expected   = """"date(2021-10-04)""""
        actual mustBe expected
      }

      "show datetime" in {
        val cell: Cell = Cell.DateTime(OffsetDateTime.parse("2021-10-04T01:00:00+02:00"))
        val actual     = cell.show
        val expected   = """"datetime(2021-10-04T01:00+02:00)""""
        actual mustBe expected
      }

      "show vec of primitives" in {
        val cell: Cell = Cell.Vec(List[Cell](Cell.I32(1), Cell.I64(1000L), Cell.Str("alice")))
        val actual     = cell.show
        val expected = """[
                         |  "i32(1)", 
                         |  "i64(1000)", 
                         |  "str(alice)"
                         |]""".stripMargin
        actual mustBe expected
      }

      "show struct" in {
        val cell: Cell = Cell.Struct(Map("a" -> Cell.I32(1), "b" -> Cell.Str("alice")))
        val actual     = cell.show
        val expected = s"""{
                          |  "a": "i32(1)",
                          |  "b": "str(alice)"
                          |}""".stripMargin
        actual mustBe expected
      }

      "show nested struct" in {
        val cell: Cell = Cell.Struct(Map("a" -> Cell.I32(1), "b" -> Cell.Struct(Map("c" -> Cell.Str("alice")))))
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
        val cell: Cell = Cell.Vec(
          List[Cell](
            Cell.Struct(Map("a" -> Cell.I32(1), "b" -> Cell.Str("alice"))),
            Cell.Struct(Map("a" -> Cell.I32(2), "b" -> Cell.Str("bob")))
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
