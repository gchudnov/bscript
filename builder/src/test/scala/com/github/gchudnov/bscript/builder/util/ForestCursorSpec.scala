package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.TestSpec

final class ForestCursorSpec extends TestSpec:

  final case class Node(name: String)

  "ForestCursorSpec" when {
    "empty" should {
      "have the expected structure" in {
        val c = ForestCursor.empty(a => Node(a))

        // no-op

        c.forest.isEmpty mustBe (true)
        c.current mustBe (None)
        c.counter mustBe Vector.empty[Int]
        c.level mustBe (-1)
      }
    }

    "push" should {
      "add a node at layer 0" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push()

        actual.forest.size mustBe (1)
        actual.current mustBe Some(Node("a"))
        actual.counter mustBe Vector(0)
        actual.level mustBe (0)
      }
    }

    "push, pop" should {
      "add a node at layer 0" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().pop()

        actual.forest.size mustBe (1)
        actual.current mustBe None
        actual.counter mustBe Vector(0)
        actual.level mustBe (-1)
      }
    }    

    "push, push" should {
      "add node at layers 0, 1" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().push()

        actual.forest.size mustBe (2)
        actual.current mustBe Some(Node("a.a"))
        actual.counter mustBe Vector(0, 0)
        actual.level mustBe (1)
      }
    }

    "push, push, push" should {
      "add nodes at layers 0, 1, 2" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().push().push()

        actual.forest.size mustBe (3)
        actual.current mustBe Some(Node("a.a.a"))
        actual.counter mustBe Vector(0, 0, 0)
        actual.level mustBe (2)
      }
    }
  }
