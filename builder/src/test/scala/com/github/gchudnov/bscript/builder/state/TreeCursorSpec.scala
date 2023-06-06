package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.state.TreeCursor

/**
 * Tree Cursor Specification
 */
final class TreeCursorSpec extends TestSpec:

  final case class Node(name: String)

  "TreeCursorSpec" when {
    "empty" should {
      "have the expected structure" in {
        val c = TreeCursor.empty(a => Node(a))

        // no-op

        c.tree.isEmpty mustBe (true)
        c.at mustBe (None)
      }
    }

    /**
     * {{{
     *    a
     * }}}
     */
    "push" should {
      "add a node at layer 0" in {
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push()

        actual.tree.vertexSize mustBe (1)
        actual.at mustBe Some(Node("a"))
      }
    }

    /**
     * {{{
     *   a
     * }}}
     */
    "push, back" should {
      "add a node at layer 0" in {
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push().back()

        actual.tree.vertexSize mustBe (1)
        actual.at mustBe None
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
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push().push()

        actual.tree.vertexSize mustBe (2)
        actual.at mustBe Some(Node("a.a"))
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
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push().push().push()

        actual.tree.vertexSize mustBe (3)
        actual.at mustBe Some(Node("a.a.a"))
      }
    }

    /**
     * {{{
     *      x     | <- .at
     *   a  b  c
     * }}}
     */
    "push, back, push, back, push, back" should {
      "add nodes at layer 0 only" in {
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push().back().push().back().push().back()

        actual.tree.vertexSize mustBe (3)
        actual.at mustBe None
      }
    }

    /**
     * {{{
     *      a      | <- .at
     *    / | \
     *   /  |  \
     * a.a a.b a.c
     * }}}
     */
    "push, push, back, push, back, push, back" should {
      "add nodes at layers 0, 1" in {
        val c = TreeCursor.empty(a => Node(a))

        val actual = c.push().push().back().push().back().push().back()

        actual.tree.vertexSize mustBe (4)
        actual.at mustBe Some(Node("a"))
      }
    }
  }