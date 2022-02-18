package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.interpreter.TestSpec

final class DiffSpec extends TestSpec:
  import Diff.*

  sealed trait Box
  final case class IntBox(n: Int)    extends Box
  final case class StrBox(s: String) extends Box

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

        z.toList mustBe List(Removed("b", 2))
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

        z.toList must contain theSameElementsAs List(Updated("1", "foo", "baz"), Removed("3", "baz"), Added("4", "boo"))
      }
    }

    "with custom case classes as values" should {
      "find out that nothing has changed" in {
        val x = Map("a" -> IntBox(1))
        val y = Map("a" -> IntBox(1))

        val z = Diff.calc(x, y)

        z.isEmpty mustBe (true)
      }

      "find out when a value was added" in {
        val x = Map("a" -> IntBox(1))
        val y = Map("a" -> IntBox(1), "foo" -> StrBox("A"))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Added("foo", StrBox("A")))
      }

      "find out when values were removed" in {
        val x = Map("a" -> IntBox(1), "foo" -> StrBox("A"))
        val y = Map("a" -> IntBox(1))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Removed("foo", StrBox("A")))
      }

      "find out when values were updated" in {
        val x = Map("a" -> IntBox(1), "foo" -> StrBox("A"))
        val y = Map("a" -> IntBox(2), "foo" -> StrBox("B"))

        val z = Diff.calc(x, y)

        z.toList mustBe List(Updated("a", IntBox(1), IntBox(2)), Updated("foo", StrBox("A"), StrBox("B")))
      }
    }
  }
