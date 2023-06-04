package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.util.Resources
import com.github.gchudnov.bscript.interpreter.memory.Path
import com.github.gchudnov.bscript.interpreter.memory.Area

final class AreaSpec extends TestSpec:

  /**
   * {{{
   *   struct A {
   *     x: i32
   *     b: {
   *       y: i32
   *     },
   *     c: [i32; 3],
   *     d: [{ x: i32, y: i32 }; 2],
   *   };
   *
   *   // globals
   *   {
   *      let a: i32 = 10;
   *      let b: i32 = 20;
   *
   *      // locals
   *      {
   *         let a: A = {
   *           x: 0,
   *           b: {
   *             y: 3
   *           },
   *           c: [1, 2, 3],
   *           d: [
   *             { x: 1, y: 2 },
   *             { x: 3, y: 4 },
   *           ]
   *         };
   *
   *         // main
   *         fn main() -> void {
   *           let c: f64 = 12.34;
   *         }
   *      }
   *   }
   * }}}
   */
  "Area" when {
    val aStruct = Cell.Struct(
      "x" -> Cell.i32(0),
      "b" -> Cell.struct("y" -> Cell.I32(3)),
      "c" -> Cell.vec(Cell.I32(1), Cell.I32(2), Cell.I32(3)),
      "d" -> Cell.vec(
        Cell.struct("x" -> Cell.I32(1), "y" -> Cell.I32(2)),
        Cell.struct("x" -> Cell.I32(3), "y" -> Cell.I32(4)),
      ),
    )

    val globals = Area("globals", Map("a" -> Cell.I32(10), "b" -> Cell.I32(20)))
    val locals  = Area("locals", Map("a" -> aStruct), Some(globals))
    val main    = Area("main", Map("c" -> Cell.F64(12.34)), Some(locals))

    "isEmpty" should {

      "return true if there are no members in the area" in {
        val a = Area("area")

        a.isEmpty mustBe (true)
      }

      "return false if there are members in the area" in {
        val a = Area("area", Map("a" -> Cell.I32(10)))

        a.isEmpty mustBe (false)
      }
    }

    "get by name" should {

      "return a cell by its id if the cell exists" in {
        val optCell = globals.get("a")
        optCell match
          case None =>
            fail("should be 'some")
          case Some(actual) =>
            actual mustBe Cell.I32(10)
      }

      "return a cell from a parent if the cell exists" in {
        val optCell = locals.get("b")
        optCell match
          case None =>
            fail("should be 'some")
          case Some(actual) =>
            actual mustBe Cell.I32(20)
      }

      "fail if the cell does not exist" in {
        val optCell = locals.get("xxx")
        optCell match
          case None =>
            succeed
          case Some(actual) =>
            fail("should be 'none")
      }
    }

    "tryGet by name" should {

      "return a cell by its id if the cell exists" in {
        val errOrCell = globals.tryGet("a")
        errOrCell match
          case Left(_) =>
            fail("should be 'right")
          case Right(actual) =>
            actual mustBe Cell.I32(10)
      }

      "return a cell from a parent if the cell exists" in {
        val errOrCell = locals.tryGet("b")
        errOrCell match
          case Left(_) =>
            fail("should be 'right")
          case Right(actual) =>
            actual mustBe Cell.I32(20)
      }

      "fail if the cell does not exist" in {
        val errOrCell = locals.tryGet("xxx")
        errOrCell match
          case Left(_) =>
            succeed
          case Right(actual) =>
            fail("should be 'left")
      }
    }

    "get by path" should {

      "return a cell by its path if the path exists" in {
        val optCell = locals.get(Path(List("a", "b", "y")))
        optCell match
          case None =>
            fail("should be 'some")
          case Some(actual) =>
            actual mustBe Cell.I32(3)
      }

      "return no value if the path is invalid" in {
        val optCell = locals.get(Path(List("a", "b", "y", "z")))
        optCell match
          case None =>
            succeed
          case Some(actual) =>
            fail("should be 'none")
      }

      "retrun no value if the path is broken" in {
        // NOTE: a.b.y is a valid path, but a.b is not
        val optCell = locals.get(Path(List("a", "y")))
        optCell match
          case None =>
            succeed
          case Some(actual) =>
            fail("should be 'none")
      }

      "return value from the array" in {
        val optCell = locals.get(Path(List("a", "c", "1")))
        optCell match
          case None =>
            fail("should be 'some")
          case Some(actual) =>
            actual mustBe Cell.I32(2)
      }

      "return value from a struct in the array" in {
        val optCell = locals.get(Path(List("a", "d", "1", "y")))
        optCell match
          case None =>
            fail("should be 'some")
          case Some(actual) =>
            actual mustBe Cell.I32(4)
      }
    }

    "tryGet by path" should {
      "return a cell by its path" in {
        val errOrCell = locals.tryGet(Path(List("a", "b", "y")))
        errOrCell match
          case Left(_) =>
            fail("should be 'right")
          case Right(actual) =>
            actual mustBe Cell.I32(3)
      }
    }

    "put" should {
      "update area" in {
        val updated = globals.put("a", Cell.I32(20))

        Area.diff(globals, updated) match
          case Left(_) =>
            fail("should be 'right")
          case Right(diff) =>
            diff mustBe List(Diff.Updated(Path(List("globals", "a")), Cell.I32(10), Cell.I32(20)))
      }
    }

    "update by name" should {

      "return a new memory area hierarchy when update an element in the deepest memory area" in {
        val updated = main.update("c", Cell.F64(56.78))

        updated match
          case None =>
            fail("should be 'some")
          case Some(u) =>
            Area.diff(main, u) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("main.c"), Cell.F64(12.34), Cell.F64(56.78)),
                )
      }

      "return a new memory area hierarchy when add an element in the parent memory area" in {
        val updated = main.update("b", Cell.I32(30))

        updated match
          case None =>
            fail("should be 'some")
          case Some(u) =>
            Area.diff(main, u) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("main.locals.globals.b"), Cell.I32(20), Cell.I32(30)),
                )
      }

      "return a new memory area hierarchy when add an element in the child memory area" in {
        val updated = main.update("c", Cell.F32(20.1f))

        updated match
          case None =>
            fail("should be 'some")
          case Some(u) =>
            Area.diff(main, u) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("main.c"), Cell.F64(12.34), Cell.F32(20.1f)),
                )
      }
    }

    "tryUpdate by name" should {

      "update the cell by name" in {
        val errOrUpd = main.tryUpdate("a", Cell.I32(20))
        errOrUpd match
          case Left(_) =>
            fail("should be 'right")
          case Right(u) =>
            Area.diff(main, u) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("main.locals.a"), aStruct, Cell.I32(20)),
                )
      }
    }

    "tryUpdate by path" should {

      "modify cell by its path" in {
        val errOrUpd = locals.tryUpdate(Path(List("a", "b", "y")), Cell.I32(12))
        errOrUpd match
          case Left(_) =>
            fail("should be 'right")
          case Right(updated) =>
            Area.diff(locals, updated) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                val newStruct = Cell.Struct(
                  "x" -> Cell.i32(0),
                  "b" -> Cell.struct("y" -> Cell.I32(12)),
                  "c" -> Cell.vec(Cell.I32(1), Cell.I32(2), Cell.I32(3)),
                  "d" -> Cell.vec(
                    Cell.struct("x" -> Cell.I32(1), "y" -> Cell.I32(2)),
                    Cell.struct("x" -> Cell.I32(3), "y" -> Cell.I32(4)),
                  ),
                )

                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("locals.a"), aStruct, newStruct),
                )
      }

      "modify cell by its path if path has only one part" in {
        val errOrUpd = locals.tryUpdate(Path(List("a")), Cell.I32(22))
        errOrUpd match
          case Left(_) =>
            fail("should be 'right")
          case Right(updated) =>
            Area.diff(locals, updated) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                val expected = Cell.I32(22)

                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("locals.a"), aStruct, expected),
                )
      }

      "modify array" in {
        val errOrUpd = locals.tryUpdate(Path(List("a", "c", "1")), Cell.I32(22))
        errOrUpd match
          case Left(_) =>
            fail("should be 'right")
          case Right(updated) =>
            Area.diff(locals, updated) match
              case Left(_) =>
                fail("should be 'right")
              case Right(diff) =>
                val expected = Cell.Struct(
                  "x" -> Cell.i32(0),
                  "b" -> Cell.struct("y" -> Cell.I32(3)),
                  "c" -> Cell.vec(Cell.I32(1), Cell.I32(22), Cell.I32(3)),
                  "d" -> Cell.vec(
                    Cell.struct("x" -> Cell.I32(1), "y" -> Cell.I32(2)),
                    Cell.struct("x" -> Cell.I32(3), "y" -> Cell.I32(4)),
                  ),
                )

                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("locals.a"), aStruct, expected),
                )
      }

      "fail to modify if the path is too long" in {
        val errOrUpd = locals.tryUpdate(Path(List("a", "b", "y", "z")), Cell.I32(12))
        errOrUpd match
          case Left(t) =>
            t.getMessage.contains("doesn't have fields to fetch") mustBe (true)
          case Right(_) =>
            fail("should be 'left")
      }

      "fail to modify if the path is empty" in {
        val errOrUpd = locals.tryUpdate(Path.empty, Cell.I32(12))
        errOrUpd match
          case Left(t) =>
            t.getMessage.contains("Path to update a Cell is empty") mustBe (true)
          case Right(_) =>
            fail("should be 'left")
      }
    }

    "update by path" should {

      "modify cell by its path" in {
        val maybeUpdated = locals.update(Path(List("a", "b", "y")), Cell.I32(12))
        maybeUpdated match
          case None =>
            fail("should be 'some'")
          case Some(updated) =>
            Area.diff(locals, updated) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                val newStruct = Cell.Struct(
                  "x" -> Cell.i32(0),
                  "b" -> Cell.struct("y" -> Cell.I32(12)),
                  "c" -> Cell.vec(Cell.I32(1), Cell.I32(2), Cell.I32(3)),
                  "d" -> Cell.vec(
                    Cell.struct("x" -> Cell.I32(1), "y" -> Cell.I32(2)),
                    Cell.struct("x" -> Cell.I32(3), "y" -> Cell.I32(4)),
                  ),
                )

                diff must contain theSameElementsAs List(
                  Diff.Updated(Path.parse("locals.a"), aStruct, newStruct),
                )
      }
    }

    "pop" should {
      val aStruct = Cell.Struct(
        "x" -> Cell.I32(0),
        "b" -> Cell.Struct("y" -> Cell.I32(0)),
      )

      val globals = Area("globals", Map("a" -> Cell.I32(10)))
      val locals  = Area("locals", Map("a" -> aStruct), Some(globals))

      "return the parent" in {
        val actual = locals.pop()

        actual mustBe (Some(globals))
      }
    }

    "name prefix" should {
      "be appended to the name in the diff results" in {
        val changes = List(Diff.Updated("1", "foo", "baz"), Diff.Removed("3", "foo"), Diff.Added("4", "boo"))
        val updated = changes.map(Area.withPrefix(Path(List("parent")), _))

        val keys = updated.map(_.key)

        keys must contain allElementsOf List(Path.parse("parent.1"), Path.parse("parent.3"), Path.parse("parent.4"))
      }
    }

    "Area diff" should {

      "return an error if spaces have different names" in {
        val spaceBefore = Area("SPACE-A", Map.empty[String, Cell], None)
        val spaceAfter  = Area("SPACE-B", Map.empty[String, Cell], None)

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff.isLeft mustBe (true)
      }

      "return an error if parent spaces have different names" in {
        val parentBefore = Area("PARENT-A", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = Area("PARENT-B", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = Area("s", Map.empty[String, Cell], Some(parentBefore))
        val spaceAfter  = Area("s", Map.empty[String, Cell], Some(parentAfter))

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff.isLeft mustBe (true)
      }

      "return no changes if spaces are empty without parents" in {
        val spaceBefore = Area("s", Map.empty[String, Cell], None)
        val spaceAfter  = Area("s", Map.empty[String, Cell], None)

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are empty with parents with the same data" in {
        val parentBefore = Area("p", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = Area("p", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = Area("s", Map.empty[String, Cell], Some(parentBefore))
        val spaceAfter  = Area("s", Map.empty[String, Cell], Some(parentAfter))

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are the same without parents" in {
        val spaceBefore = Area("s", Map("a" -> Cell.I32(10)), None)
        val spaceAfter  = Area("s", Map("a" -> Cell.I32(10)), None)

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are the same with the same data in parents" in {
        val parentBefore = Area("p", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = Area("p", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = Area("s", Map("a" -> Cell.I32(23)), Some(parentBefore))
        val spaceAfter  = Area("s", Map("a" -> Cell.I32(23)), Some(parentAfter))

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return changes if space was updated" in {
        val spaceBefore = Area("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), None)
        val spaceAfter  = Area("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), None)

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Updated(Path.parse("s.x"), Cell.I32(1), Cell.I32(2)),
              Diff.Removed(Path.parse("s.y"), Cell.F32(1.2f)),
              Diff.Added(Path.parse("s.z"), Cell.Str("str")),
            )
      }

      "return changes if space was updated including updates in parents" in {
        val parentBefore = Area("p", Map("x" -> Cell.Str("A"), "u" -> Cell.I64(1L)), None)
        val parentAfter  = Area("p", Map("x" -> Cell.Str("B"), "struct" -> Cell.Struct(Map("x" -> Cell.Str("alice")))), None)

        val spaceBefore = Area("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), Some(parentBefore))
        val spaceAfter  = Area("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), Some(parentAfter))

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Updated(Path.parse("s.p.x"), Cell.Str("A"), Cell.Str("B")),
              Diff.Removed(Path.parse("s.p.u"), Cell.I64(1L)),
              Diff.Added(Path.parse("s.p.struct"), Cell.Struct(Map("x" -> Cell.Str("alice")))),
              Diff.Updated(Path.parse("s.x"), Cell.I32(1), Cell.I32(2)),
              Diff.Removed(Path.parse("s.y"), Cell.F32(1.2f)),
              Diff.Added(Path.parse("s.z"), Cell.Str("str")),
            )
      }

      "return changes if parent before was absent" in {
        val parentAfter = Area("p", Map("x" -> Cell.Str("B"), "struct" -> Cell.Struct(Map("x" -> Cell.Str("alice")))), None)

        val spaceBefore = Area("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), None)
        val spaceAfter  = Area("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), Some(parentAfter))

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Added(Path.parse("s.p.x"), Cell.Str("B")),
              Diff.Added(Path.parse("s.p.struct"), Cell.Struct(Map("x" -> Cell.Str("alice")))),
              Diff.Updated(Path.parse("s.x"), Cell.I32(1), Cell.I32(2)),
              Diff.Removed(Path.parse("s.y"), Cell.F32(1.2f)),
              Diff.Added(Path.parse("s.z"), Cell.Str("str")),
            )
      }

      "return changes if parent after is absent" in {
        val parentBefore = Area("p", Map("x" -> Cell.Str("A"), "u" -> Cell.I64(1L)), None)

        val spaceBefore = Area("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), Some(parentBefore))
        val spaceAfter  = Area("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), None)

        val errOrDiff = Area.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Removed(Path.parse("s.p.x"), Cell.Str("A")),
              Diff.Removed(Path.parse("s.p.u"), Cell.I64(1L)),
              Diff.Updated(Path.parse("s.x"), Cell.I32(1), Cell.I32(2)),
              Diff.Removed(Path.parse("s.y"), Cell.F32(1.2f)),
              Diff.Added(Path.parse("s.z"), Cell.Str("str")),
            )
      }
    }

    "show" should {
      "display the struct" in {
        val xStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.struct("y" -> Cell.I32(0)),
        )

        val globals = Area("globals", Map("a" -> Cell.I32(10)))
        val locals  = Area("locals", Map("a" -> xStruct), Some(globals))

        val expected = Resources.asString("data/mem-space-2.json").toTry.get
        val actual   = locals.show

        actual.trim mustBe (expected.trim)
      }

      "display the complex struct" in {
        val expected = Resources.asString("data/mem-space-3.json").toTry.get
        val actual   = locals.show

        actual.trim mustBe (expected.trim)
      }
    }
  }
