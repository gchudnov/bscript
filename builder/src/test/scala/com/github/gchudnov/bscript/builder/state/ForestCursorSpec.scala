package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec

/**
  * Forest Cursor Specification
  * 
  * TODO: understand what this is doing
  */
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
        c.level mustBe (0)
      }
    }

    /**
     * {{{
     *    a
     * }}}
     */
    "push" should {
      "add a node at layer 0" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push()

        actual.forest.size mustBe (1)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a")))
        actual.forest.edges must contain theSameElementsAs (Map.empty[Node, Node])
        actual.current mustBe Some(Node("a"))
        actual.counter mustBe Vector(0)
        actual.level mustBe (1)
      }
    }

    /**
     * {{{
     *   a
     * }}}
     */
    "push, pop" should {
      "add a node at layer 0" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().pop()

        actual.forest.size mustBe (1)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a")))
        actual.forest.edges must contain theSameElementsAs (Map.empty[Node, Node])
        actual.current mustBe None
        actual.counter mustBe Vector(0)
        actual.level mustBe (0)
      }
    }

    /**
     * {{{
     *    a
     *    |
     *   a.a
     * }}}
     */
    "push, push" should {
      "add node at layers 0, 1" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().push()

        actual.forest.size mustBe (2)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a"), Node("a.a")))
        actual.forest.edges must contain theSameElementsAs (Map(Node("a.a") -> Node("a")))
        actual.current mustBe Some(Node("a.a"))
        actual.counter mustBe Vector(0, 0)
        actual.level mustBe (2)
      }
    }

    /**
     * {{{
     *    a
     *    |
     *   a.a
     *    |
     *  a.a.a
     * }}}
     */
    "push, push, push" should {
      "add nodes at layers 0, 1, 2" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().push().push()

        actual.forest.size mustBe (3)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a"), Node("a.a"), Node("a.a.a")))
        actual.forest.edges must contain theSameElementsAs (Map(Node("a.a") -> Node("a"), Node("a.a.a") -> Node("a.a")))
        actual.current mustBe Some(Node("a.a.a"))
        actual.counter mustBe Vector(0, 0, 0)
        actual.level mustBe (3)
      }
    }

    /**
     * {{{
     *   a  b  c
     * }}}
     */
    "push, pop, push, pop, push, pop" should {
      "add nodes at layer 0 only" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().pop().push().pop().push().pop()

        actual.forest.size mustBe (3)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a"), Node("b"), Node("c")))
        actual.forest.edges must contain theSameElementsAs (Map.empty[Node, Node])
        actual.current mustBe None
        actual.counter mustBe Vector(2)
        actual.level mustBe (0)
      }
    }

    /**
     * {{{
     *      a
     *    / | \
     *   /  |  \
     * a.a a.b a.c
     * }}}
     */
    "push, push, pop, push, pop, push, pop" should {
      "add nodes at layers 0, 1" in {
        val c = ForestCursor.empty(a => Node(a))

        val actual = c.push().push().pop().push().pop().push().pop()

        actual.forest.size mustBe (4)
        actual.forest.vertices must contain theSameElementsAs (Set(Node("a"), Node("a.a"), Node("a.b"), Node("a.c")))
        actual.forest.edges must contain theSameElementsAs (Map(Node("a.a") -> Node("a"), Node("a.b") -> Node("a"), Node("a.c") -> Node("a")))
        actual.current mustBe Some(Node("a"))
        actual.counter mustBe Vector(0, 2)
        actual.level mustBe (1)
      }
    }
  }
