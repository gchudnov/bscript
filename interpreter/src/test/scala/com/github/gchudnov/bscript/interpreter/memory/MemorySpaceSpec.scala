package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.util.Resources
import com.github.gchudnov.bscript.interpreter.memory.Path

final class MemorySpaceSpec extends TestSpec:

  "MemorySpace" when {

    "created" should {
      "be empty" in {
        val m = MemorySpace("globals")

        m.members.isEmpty mustBe (true)
      }
    }

    /**
     * {{{
     *   struct A {
     *     int x;
     *     B b;
     *   };
     *   struct B { int y; };
     *
     *   A a;
     *
     *   a.b.y; // here 'a.b.y' is a path
     * }}}
     */
    "fetch" should {
      val aStruct = Cell.Struct(
        "x" -> Cell.i32(0),
        "b" -> Cell.struct("y" -> Cell.I32(3))
      )

      val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
      val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

      "return a cell by its path if the path exists" in {
        val optCell = locals.fetch(Path("a.b.y"))
        optCell match
          case None =>
            fail("should be 'right")
          case Some(actual) =>
            actual mustBe Cell.I32(3)
      }

      "return no value if the path is invalid" in {
        val optCell = locals.fetch(Path("a.b.y.z"))
        optCell match
          case None =>
            succeed
          case Some(actual) =>
            fail("should be 'left")
      }
    }

    "tryFetch" should {

      /**
       * {{{
       *   struct A {
       *     int x;
       *     B b;
       *   };
       *   struct B { int y; };
       *
       *   A a;
       *
       *   a.b.y; // here 'a.b.y' is a path
       * }}}
       */
      "return a cell by its path" in {
        val aStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.Struct("y" -> Cell.I32(3))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

        val errOrCell = locals.tryFetch(Path("a.b.y"))
        errOrCell match
          case Left(_) => fail("should be 'right")
          case Right(actual) =>
            actual mustBe Cell.I32(3)
      }
    }

    "update" should {

      /**
       * {{{
       *   globals { a: 10 }
       *   ^
       *   f { b: "B" }
       *   ^
       *   main { c: 12.34 }
       * }}}
       */
      "return a new memory space hierarchy when update an element in the deepest memory space" in {
        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val f       = MemorySpace("f", Map("b" -> Cell.Str("B")), Some(globals))
        val main    = MemorySpace("main", Map("c" -> Cell.F64(12.34)), Some(f))

        val updated = main.update("a", Cell.I32(20))

        updated match
          case None => fail("should be 'some")
          case Some(u) =>
            MemorySpace.diff(main, u) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated("main/f/globals/a", Cell.I32(10), Cell.I32(20))
                )
      }

      /**
       * {{{
       *   globals { a: 10 }
       *   ^
       *   f { b: "B" }
       *   ^
       *   main { c: 12.34 }
       * }}}
       */
      "return a new memory space hierarchy when add an element in the parent memory space" in {
        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val f       = MemorySpace("f", Map("b" -> Cell.Str("B")), Some(globals))
        val main    = MemorySpace("main", Map("c" -> Cell.F64(12.34)), Some(f))

        val updated = main.update("b", Cell.I32(20))

        updated match
          case None => fail("should be 'some")
          case Some(u) =>
            MemorySpace.diff(main, u) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated("main/f/b", Cell.Str("B"), Cell.I32(20))
                )
      }

      /**
       * {{{
       *   globals { a: 10 }
       *   ^
       *   f { b: "B" }
       *   ^
       *   main { c: 12.34 }
       * }}}
       */
      "return a new memory space hierarchy when add an element in the child memory space" in {
        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val f       = MemorySpace("f", Map("b" -> Cell.Str("B")), Some(globals))
        val main    = MemorySpace("main", Map("c" -> Cell.F64(12.34)), Some(f))

        val updated = main.update("c", Cell.F32(20.1f))

        updated match
          case None => fail("should be 'some")
          case Some(u) =>
            MemorySpace.diff(main, u) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                diff must contain theSameElementsAs List(
                  Diff.Updated("main/c", Cell.F64(12.34), Cell.F32(20.1f))
                )
      }
    }

    "tryPatch" should {

      /**
       * {{{
       *   struct A {
       *     int x;
       *     B b;
       *   };
       *   struct B { int y; };
       *
       *   A a;
       *
       *   a.b.y = 2; // here 'a.b.y' is a path
       * }}}
       */
      "modify cell by its path" in {
        val initStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.Struct("y" -> Cell.I32(0))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> initStruct), Some(globals))

        val errOrUpd = locals.tryPatch(Path("a.b.y"), Cell.I32(12))
        errOrUpd match
          case Left(_) => fail("should be 'right")
          case Right(updated) =>
            MemorySpace.diff(locals, updated) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                val newStruct = Cell.Struct(
                  "x" -> Cell.I32(0),
                  "b" -> Cell.Struct("y" -> Cell.I32(12))
                )

                diff must contain theSameElementsAs List(
                  Diff.Updated("locals/a", initStruct, newStruct)
                )
      }

      /**
       * {{{
       *   struct A {
       *     int x;
       *     B b;
       *   };
       *   struct B { int y; };
       *
       *   A a;
       *
       *   a = 2; // here 'a' is a path ; we update it to '2', it is wrong from the perspective of an interpreter, but OK for memory-spaces.
       * }}}
       */
      "modify cell by its path if path has only one part" in {
        val aStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.Struct("y" -> Cell.I32(0))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

        val errOrUpd = locals.tryPatch(Path("a"), Cell.I32(22))
        errOrUpd match
          case Left(_) => fail("should be 'right")
          case Right(updated) =>
            MemorySpace.diff(locals, updated) match
              case Left(_) => fail("should be 'right")
              case Right(diff) =>
                val expected = Cell.I32(22)

                diff must contain theSameElementsAs List(
                  Diff.Updated("locals/a", aStruct, expected)
                )
      }

      "modify cell by its path cannot be made if the path is too long" in {
        val aStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.Struct("y" -> Cell.I32(0))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

        val errOrUpd = locals.tryPatch(Path("a.b.y.z"), Cell.I32(12))
        errOrUpd match
          case Left(t)  => t.getMessage.contains("doesn't have fields to fetch") mustBe (true)
          case Right(_) => fail("should be 'left")
      }

      "modify cell by its path cannot be made if the path is empty" in {
        val aStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.Struct("y" -> Cell.I32(0))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

        val errOrUpd = locals.tryPatch(Path(""), Cell.I32(12))
        errOrUpd match
          case Left(t)  => t.getMessage.contains("Path to update a Cell is empty") mustBe (true)
          case Right(_) => fail("should be 'left")
      }
    }

    "name prefix" should {
      "be appended to the name in the diff results" in {
        val changes = List(Diff.Updated("1", "foo", "baz"), Diff.Removed("3", "foo"), Diff.Added("4", "boo"))
        val updated = changes.map(MemorySpace.appendKeyPrefix("parent", _))

        val keys = updated.map(_.key)

        keys must contain allElementsOf List("parent/1", "parent/3", "parent/4")
      }
    }

    "MemorySpace diff" should {

      "return an error if spaces have different names" in {
        val spaceBefore = MemorySpace("SPACE-A", Map.empty[String, Cell], None)
        val spaceAfter  = MemorySpace("SPACE-B", Map.empty[String, Cell], None)

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff.isLeft mustBe (true)
      }

      "return an error if parent spaces have different names" in {
        val parentBefore = MemorySpace("PARENT-A", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = MemorySpace("PARENT-B", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = MemorySpace("s", Map.empty[String, Cell], Some(parentBefore))
        val spaceAfter  = MemorySpace("s", Map.empty[String, Cell], Some(parentAfter))

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff.isLeft mustBe (true)
      }

      "return no changes if spaces are empty without parents" in {
        val spaceBefore = MemorySpace("s", Map.empty[String, Cell], None)
        val spaceAfter  = MemorySpace("s", Map.empty[String, Cell], None)

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are empty with parents with the same data" in {
        val parentBefore = MemorySpace("p", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = MemorySpace("p", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = MemorySpace("s", Map.empty[String, Cell], Some(parentBefore))
        val spaceAfter  = MemorySpace("s", Map.empty[String, Cell], Some(parentAfter))

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are the same without parents" in {
        val spaceBefore = MemorySpace("s", Map("a" -> Cell.I32(10)), None)
        val spaceAfter  = MemorySpace("s", Map("a" -> Cell.I32(10)), None)

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return no changes if spaces are the same with the same data in parents" in {
        val parentBefore = MemorySpace("p", Map("b" -> Cell.Str("A")), None)
        val parentAfter  = MemorySpace("p", Map("b" -> Cell.Str("A")), None)

        val spaceBefore = MemorySpace("s", Map("a" -> Cell.I32(23)), Some(parentBefore))
        val spaceAfter  = MemorySpace("s", Map("a" -> Cell.I32(23)), Some(parentAfter))

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_)     => fail("should be 'right")
          case Right(diff) => diff.isEmpty mustBe (true)
      }

      "return changes if space was updated" in {
        val spaceBefore = MemorySpace("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), None)
        val spaceAfter  = MemorySpace("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), None)

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Updated("s/x", Cell.I32(1), Cell.I32(2)),
              Diff.Removed("s/y", Cell.F32(1.2f)),
              Diff.Added("s/z", Cell.Str("str"))
            )
      }

      "return changes if space was updated including updates in parents" in {
        val parentBefore = MemorySpace("p", Map("x" -> Cell.Str("A"), "u" -> Cell.I64(1L)), None)
        val parentAfter  = MemorySpace("p", Map("x" -> Cell.Str("B"), "struct" -> Cell.Struct(Map("x" -> Cell.Str("alice")))), None)

        val spaceBefore = MemorySpace("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), Some(parentBefore))
        val spaceAfter  = MemorySpace("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), Some(parentAfter))

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Updated("s/p/x", Cell.Str("A"), Cell.Str("B")),
              Diff.Removed("s/p/u", Cell.I64(1L)),
              Diff.Added("s/p/struct", Cell.Struct(Map("x" -> Cell.Str("alice")))),
              Diff.Updated("s/x", Cell.I32(1), Cell.I32(2)),
              Diff.Removed("s/y", Cell.F32(1.2f)),
              Diff.Added("s/z", Cell.Str("str"))
            )
      }

      "return changes if parent before was absent" in {
        val parentAfter = MemorySpace("p", Map("x" -> Cell.Str("B"), "struct" -> Cell.Struct(Map("x" -> Cell.Str("alice")))), None)

        val spaceBefore = MemorySpace("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), None)
        val spaceAfter  = MemorySpace("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), Some(parentAfter))

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Added("s/p/x", Cell.Str("B")),
              Diff.Added("s/p/struct", Cell.Struct(Map("x" -> Cell.Str("alice")))),
              Diff.Updated("s/x", Cell.I32(1), Cell.I32(2)),
              Diff.Removed("s/y", Cell.F32(1.2f)),
              Diff.Added("s/z", Cell.Str("str"))
            )
      }

      "return changes if parent after is absent" in {
        val parentBefore = MemorySpace("p", Map("x" -> Cell.Str("A"), "u" -> Cell.I64(1L)), None)

        val spaceBefore = MemorySpace("s", Map("x" -> Cell.I32(1), "y" -> Cell.F32(1.2f)), Some(parentBefore))
        val spaceAfter  = MemorySpace("s", Map("x" -> Cell.I32(2), "z" -> Cell.Str("str")), None)

        val errOrDiff = MemorySpace.diff(spaceBefore, spaceAfter)

        errOrDiff match
          case Left(_) => fail("should be 'right")
          case Right(diff) =>
            diff must contain theSameElementsAs List(
              Diff.Removed("s/p/x", Cell.Str("A")),
              Diff.Removed("s/p/u", Cell.I64(1L)),
              Diff.Updated("s/x", Cell.I32(1), Cell.I32(2)),
              Diff.Removed("s/y", Cell.F32(1.2f)),
              Diff.Added("s/z", Cell.Str("str"))
            )
      }
    }

    "displayed" should {
      "display the hierarchy" in {
        val aStruct = Cell.Struct(
          "x" -> Cell.I32(0),
          "b" -> Cell.struct("y" -> Cell.I32(0))
        )

        val globals = MemorySpace("globals", Map("a" -> Cell.I32(10)))
        val locals  = MemorySpace("locals", Map("a" -> aStruct), Some(globals))

        val expected = Resources.asString("data/mem-space-2.json").toTry.get
        val actual   = locals.show

        actual.trim mustBe (expected.trim)
      }
    }
  }
