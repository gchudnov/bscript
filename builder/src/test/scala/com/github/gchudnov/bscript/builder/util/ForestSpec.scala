package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.TestSpec

final class ForestSpec extends TestSpec:

  private final case class Node(name: String)

  "ForestSpec" when {

    /**
     * {{{
     *   <empty>
     * }}}
     */
    "empty" should {
      "construct an empty forest" in {
        val t0 = Forest.empty[Node]

        t0.size mustBe (0)
        t0.vertices.size mustBe (0)
        t0.edges.size mustBe (0)
      }
    }

    /**
     * {{{
     *   b0
     * }}}
     */
    "a new node is added" should {
      "be located in the forest" in {
        val b0 = Node("b0")

        val t1 = Forest
          .empty[Node]
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(b0)
        t1.edges.size mustBe (0)
      }
    }

    "a new node added multiple times" should {
      "results in one node" in {
        val b0 = Node("b0")

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(b0)
        t1.edges.size mustBe (0)
      }

      "results in the same tree" in {
        val b0 = Node("b0")
        val b1 = Node("b1")
        val b2 = Node("b2")

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(b0)
        t1.edges.size mustBe (0)

        val t2 = t1
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)
          .link(b1, b0)
          .link(b2, b0)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(b0, b1, b2)
        t2.edges.size mustBe (2)
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

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(b0, b1, b2)
        t1.edges.size mustBe (2)
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

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .add(b3)
          .link(b1, b0)
          .link(b2, b0)
          .link(b3, b2)

        t1.vertices.size mustBe (4)
        t1.vertices must contain allElementsOf List(b0, b1, b2, b3)
        t1.edges.size mustBe (3)

        val p1 = t1.parentOf(b3)
        p1 mustBe (Some(b2))

        val p2 = t1.parentOf(b2)
        p2 mustBe (Some(b0))

        val p3 = t1.parentOf(b1)
        p3 mustBe (Some(b0))

        p2 mustBe (p3)

        val p4 = t1.parentOf(b0)
        p4 mustBe (None)
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

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(b0, b1, b2)
        t1.edges.size mustBe (2)
        t1.parentOf(b1) mustBe (Some(b0))

        val t2 = t1.link(b1, b2)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(b0, b1, b2)
        t2.edges.size mustBe (2)
        t2.parentOf(b1) mustBe (Some(b2))
      }
    }

    "maybeLink" should {
      "link if the destination is specified" in {
        val b0 = Node("b0")
        val b1 = Node("b1")

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .maybeLink(b1, Some(b0))

        t1.vertices.size mustBe (2)
        t1.vertices must contain allElementsOf List(b0, b1)
        t1.edges.size mustBe (1)
        t1.parentOf(b1) mustBe (Some(b0))
      }

      "do not link of the destination is not specified" in {
        val b0 = Node("b0")
        val b1 = Node("b1")

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .maybeLink(b1, None)

        t1.vertices.size mustBe (2)
        t1.vertices must contain allElementsOf List(b0, b1)
        t1.edges.size mustBe (0)
        t1.parentOf(b1) mustBe None
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

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(b0, b1, b2)
        t1.edges.size mustBe (2)

        val t2 = t1
          .add(b3)
          .replace(b1, b3)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(b0, b3, b2)
        t2.edges.size mustBe (2)
      }

      /**
       * {{{
       *     b0  ->  b1
       * }}}
       */
      "replace root only" in {
        val b0 = Node("b0")
        val b1 = Node("b1")

        val t1 = Forest
          .empty[Node]
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(b0)
        t1.edges.size mustBe (0)

        val t2 = t1.replace(b0, b1)

        t2.vertices.size mustBe (1)
        t2.vertices must contain allElementsOf List(b1)
        t2.edges.size mustBe (0)
      }
    }

    "same name of the node" should {

      /**
       * {{{
       *        b0                              b0
       *       /  \                             |
       *     b1'  b1''  <-- false, it will be:  b1
       * }}}
       */
      "is a duplicate and not allowed" in {
        val b0 = Node("b0")
        val b1 = Node("b1") // (1) |
        val b2 = Node("b1") // (2) |

        val t1 = Forest
          .empty[Node]
          .add(b0)
          .add(b1)
          .add(b2)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (2)
        t1.vertices must contain allElementsOf List(b0, b1)
        t1.edges.size mustBe (1)
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

        val t1 = Forest
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
      }
    }
  }
