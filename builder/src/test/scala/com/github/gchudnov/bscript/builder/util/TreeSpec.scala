package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.util.Tree

final class TreeSpec extends TestSpec:

  private final case class Node(name: String)

  private object Node:
    given nodeShow: Show[Node] = new Show[Node]:
      override def show(a: Node): String =
        s"node(${a.name})"

  "TreeSpec" when {

    /**
     * {{{
     *   <empty>
     * }}}
     */
    "empty" should {
      "construct an empty tree" in {
        val t0 = Tree.empty[Node]

        t0.vertexSize mustBe (0)
      }
    }

    /**
     * {{{
     *   b0
     * }}}
     */
    "a new node is added" should {
      "be located in the tree" in {
        val b0 = Node("b0")

        val t1 = Tree
          .empty[Node]
          .add(b0)

        t1.vertexSize mustBe (1)
        t1.contains(b0) mustBe (true)
      }
    }

    "a new node added multiple times" should {

      /**
       * {{{
       *      b0
       * }}}
       */
      "raise an exception" in {
        val b0 = Node("b0")

        intercept[IllegalArgumentException] {
          Tree
            .empty[Node]
            .add(b0)
            .add(b0)
        }
      }

      /**
       * {{{
       *      b0
       *     /  \
       *    b1  b2
       * }}}
       */
      "results in the same tree" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")

        val t1 = Tree
          .empty[Node]
          .add(b0)

        t1.vertexSize mustBe (1)
        t1.contains(b0) mustBe (true)

        val t2 = t1
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)
          .link(b1, b0)
          .link(b2, b0)

        t2.vertexSize mustBe (3)

        t2.contains(b0) mustBe (true)
        t2.contains(b1) mustBe (true)
        t2.contains(b2) mustBe (true)

        t2.parentOf(b0) mustBe (None)
        t2.parentOf(b1) mustBe (Some(b0))
        t2.parentOf(b2) mustBe (Some(b0))
      }
    }

    /**
     * {{{
     *      b0
     *     /  \
     *    b1  b2
     * }}}
     */
    "tree" should {
      "be constructed of several nodes" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")

        val t1 =
          Tree
            .empty[Node]
            .add(b0)
            .add(b1)
            .add(b2)
            .link(b1, b0)
            .link(b2, b0)

        t1.vertexSize mustBe (3)

        t1.contains(b0) mustBe (true)
        t1.contains(b1) mustBe (true)
        t1.contains(b2) mustBe (true)

        t1.parentOf(b0) mustBe (None)
        t1.parentOf(b1) mustBe (Some(b0))
        t1.parentOf(b2) mustBe (Some(b0))
      }
    }

    /**
     * {{{
     *      b0
     *     /  \
     *    b1  b2
     *         \
     *          b3
     * }}}
     */
    "parent" should {
      "be located" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")
        val b3 = Node("b3")

        val t1 = Tree
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .add(b3)
          .link(b1, b0)
          .link(b2, b0)
          .link(b3, b2)

        t1.vertexSize mustBe (4)

        t1.contains(b0) mustBe (true)
        t1.contains(b1) mustBe (true)
        t1.contains(b2) mustBe (true)
        t1.contains(b3) mustBe (true)

        t1.parentOf(b0) mustBe (None)
        t1.parentOf(b1) mustBe (Some(b0))
        t1.parentOf(b2) mustBe (Some(b0))
        t1.parentOf(b3) mustBe (Some(b2))
      }
    }

    /**
     * {{{
     *      b0          b0
     *     /  \    ->    \
     *    b1  b2         b2
     *                     \
     *                     b1
     * }}}
     */
    "link" should {
      "allow to relink" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")

        val t1 = Tree
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        val expectedTree1Str = """
                                 |{
                                 |  "vertices": ["node(b0)","node(b1)","node(b2)"],
                                 |  "edges": [["node(b1)","node(b0)"],["node(b2)","node(b0)"]]
                                 |}
                                 |""".stripMargin

        t1.show.trim mustBe (expectedTree1Str.trim)

        t1.vertexSize mustBe (3)

        t1.parentOf(b0) mustBe (None)
        t1.parentOf(b1) mustBe (Some(b0))
        t1.parentOf(b2) mustBe (Some(b0))

        val t2 = t1.link(b1, b2)

        val expectedTree2Str = """
                                 |{
                                 |  "vertices": ["node(b0)","node(b1)","node(b2)"],
                                 |  "edges": [["node(b1)","node(b2)"],["node(b2)","node(b0)"]]
                                 |}
                                 |""".stripMargin

        t2.show.trim mustBe (expectedTree2Str.trim)

        t2.parentOf(b0) mustBe (None)
        t2.parentOf(b1) mustBe (Some(b2))
        t2.parentOf(b2) mustBe (Some(b0))
      }
    }

    "replace" should {

      /**
       * {{{
       *      b0            b0
       *     /  \    ->    /  \
       *    b1  b2        b3  b2
       * }}}
       */
      "change the linking" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")
        val b3 = Node("b3")

        val t1 = Tree
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertexSize mustBe (3)

        t1.parentOf(b0) mustBe (None)
        t1.parentOf(b1) mustBe (Some(b0))
        t1.parentOf(b2) mustBe (Some(b0))

        val t2 = t1
          .add(b3) // NOTE: this add is optional
          .replace(b1, b3)

        t2.parentOf(b0) mustBe (None)
        t2.parentOf(b1) mustBe (None)
        t2.parentOf(b2) mustBe (Some(b0))
        t2.parentOf(b3) mustBe (Some(b0))
      }

      /**
       * {{{
       *     b0  ->  b1
       * }}}
       */
      "replace root only" in {
        val b0 = Node("b0")
        val b1 = Node("b1")

        val t1 = Tree
          .empty[Node]
          .add(b0)

        t1.vertexSize mustBe (1)
        t1.contains(b0) mustBe (true)

        val t2 = t1.replace(b0, b1)

        t2.vertexSize mustBe (1)
        t2.contains(b0) mustBe (false)
        t2.contains(b1) mustBe (true)
      }
    }

    "same name of the node" should {

      /**
       * It is allowed to have the same name if the node is not the same.
       *
       * {{{
       *        b0
       *       /  \
       *     b1'  b1''
       * }}}
       */
      "is a duplicate and not allowed" in {
        val b0 = Node("b0")
        val b1 = Node("b1") // (1) |
        val b2 = Node("b1") // (2) |

        val t1 = Tree
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertexSize mustBe (3)
        t1.contains(b0) mustBe (true)
        t1.contains(b1) mustBe (true)
        t1.contains(b2) mustBe (true)
      }
    }

    "path to root" should {

      /**
       * {{{
       *      b0
       *     /  \
       *    b1  b2
       *         \
       *          b3
       * }}}
       */
      "be located" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")
        val b3 = Node("b3")

        val t1 = Tree
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .add(b3)
          .link(b1, b0)
          .link(b2, b0)
          .link(b3, b2)

        val expected = List(b3, b2, b0)
        val actual   = t1.path(b3)

        actual mustBe expected

        val expectedT1Str = """
                              |{
                              |  "vertices": ["node(b0)","node(b1)","node(b2)","node(b3)"],
                              |  "edges": [["node(b1)","node(b0)"],["node(b2)","node(b0)"],["node(b3)","node(b2)"]]
                              |}
                              |""".stripMargin

        t1.show.trim mustBe (expectedT1Str.trim)
      }
    }
  }
