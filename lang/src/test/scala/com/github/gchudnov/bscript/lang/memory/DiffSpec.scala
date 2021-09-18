package com.github.gchudnov.bscript.lang.memory

import com.github.gchudnov.bscript.lang.TestSpec

final class DiffSpec extends TestSpec:
  import Diff.*

  "Diff" when {
    "two maps are empty" should {
      "return empty change-set" in {
        val x = Map.empty[String, Int]
        val y = Map.empty[String, Int]

        val z = Diff.calc(x, y)

        z.isEmpty mustBe (true)
      }
    }

    "two maps have the same values" should {
      "return no diff" in {
        val x = Map("a" -> 1)
        val y = Map("a" -> 1)

        val z = Diff.calc(x, y)

        z.isEmpty mustBe (true)
      }
    }

    "new items are added" should {
      "detect that" in {
        val x = Map("a" -> 1)
        val y = Map("a" -> 1, "b" -> 2)

        val z = Diff.calc(x, y)

        z.toList mustBe List(Added("b", 2))
      }
    }

    "items are removed" should {
      "detect that" in {
        val x = Map("a" -> 1, "b" -> 2)
        val y = Map("a" -> 1)

        val z = Diff.calc(x, y)

        z.toList mustBe List(Removed("b"))
      }
    }

    "items are updated" should {
      "detect that" in {
        val x = Map("a" -> 1)
        val y = Map("a" -> 2)

        val z = Diff.calc(x, y)

        z.toList mustBe List(Updated("a", 1, 2))
      }
    }

    "items are added, updated and deleted" should {
      "detect that" in {
        val x = Map("1" -> "foo", "2" -> "bar", "3" -> "baz")
        val y = Map("1" -> "baz", "2" -> "bar", "4" -> "boo")

        val z = Diff.calc(x, y)

        z.toList must contain theSameElementsAs List(Updated("1", "foo", "baz"), Removed("3"), Added("4", "boo"))
      }
    }

    "with custom case classes as values" should {
      "find out that nothing has changed" in {
        val x = Map("a" -> IntCell(1))
        val y = Map("a" -> IntCell(1))

        val z = Diff.calc(x, y)

        z.isEmpty mustBe (true)
      }

      "find out when a value was added" in {
        val x = Map("a" -> IntCell(1))
        val y = Map("a" -> IntCell(1), "foo" -> StrCell("A"))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Added("foo", StrCell("A")))
      }

      "find out when values were removed" in {
        val x = Map("a" -> IntCell(1), "foo" -> StrCell("A"))
        val y = Map("a" -> IntCell(1))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Removed("foo"))
      }

      "find out when values were updated" in {
        val x = Map("a" -> IntCell(1), "foo" -> StrCell("A"))
        val y = Map("a" -> IntCell(2), "foo" -> StrCell("B"))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Updated("a", IntCell(1), IntCell(2)), Updated("foo", StrCell("A"), StrCell("B")))
      }
    }
  }

  sealed trait Cell
  final case class IntCell(n: Int)
  final case class StrCell(s: String)
