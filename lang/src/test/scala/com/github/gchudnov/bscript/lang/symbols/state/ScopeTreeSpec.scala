package com.github.gchudnov.bscript.lang.symbols.state

import com.github.gchudnov.bscript.lang.TestSpec
import com.github.gchudnov.bscript.lang.symbols.{ SBlock, SMethod, SStruct }
import com.github.gchudnov.bscript.lang.util.EqWrap

final class ScopeTreeSpec extends TestSpec:

  "ScopeTree" when {

    "methods are invoked" should {

      /**
       * {{{
       *   <empty>
       * }}}
       */
      "make it empty" in {
        val t0 = ScopeTree.empty

        t0.vertices.size mustBe (0)
        t0.edges.size mustBe (0)
      }

      /**
       * {{{
       *   b0
       * }}}
       */
      "add a root" in {
        val b0 = SBlock("b0")

        val t1 = ScopeTree.empty
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(EqWrap(b0))
        t1.edges.size mustBe (0)
      }

      /**
       * {{{
       *      b0
       *     /  \
       *    b1  b2
       * }}}
       */
      "add nested blocks" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t1.edges.size mustBe (2)
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
      "find parent" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")
        val b3 = SBlock("b3")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)
          .link(b3, b2)

        t1.vertices.size mustBe (4)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2), EqWrap(b3))
        t1.edges.size mustBe (3)

        val p1 = t1.parent(b3)
        p1 mustBe (Some(b2))

        val p2 = t1.parent(b2)
        p2 mustBe (Some(b0))

        val p3 = t1.parent(b1)
        p3 mustBe (Some(b0))

        p2 mustBe (p3)

        val p4 = t1.parent(b0)
        p4 mustBe (None)
      }

      /**
       * {{{
       *      b0
       *     /  \
       *    b1  b2
       * }}}
       */
      "when added twice, scopes are not introducing anything new" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")

        val t1 = ScopeTree.empty
          .add(b0)
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(EqWrap(b0))
        t1.edges.size mustBe (0)

        val t2 = t1
          .link(b1, b0)
          .link(b2, b0)
          .link(b1, b0)
          .link(b2, b0)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t2.edges.size mustBe (2)
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
      "scopes can be relinked" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t1.edges.size mustBe (2)
        t1.parent(b1) mustBe (Some(b0))

        val t2 = t1.link(b1, b2)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t2.edges.size mustBe (2)
        t2.parent(b1) mustBe (Some(b2))
      }

      /**
       * {{{
       *      b0            b0
       *     /  \    ->    /  \
       *    b1  b2        b3  b2
       * }}}
       */
      "scopes can be replaced in a tree" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")
        val b3 = SBlock("b3")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t1.edges.size mustBe (2)

        val t2 = t1.replace(b1, b3)

        t2.vertices.size mustBe (3)
        t2.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b3), EqWrap(b2))
        t2.edges.size mustBe (2)
      }

      /**
       * {{{
       *     b0  ->  b1
       * }}}
       */
      "scopes can be replaces when only root node is present" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")

        val t1 = ScopeTree.empty
          .add(b0)

        t1.vertices.size mustBe (1)
        t1.vertices must contain allElementsOf List(EqWrap(b0))
        t1.edges.size mustBe (0)

        val t2 = t1.replace(b0, b1)

        t2.vertices.size mustBe (1)
        t2.vertices must contain allElementsOf List(EqWrap(b1))
        t2.edges.size mustBe (0)
      }

      /**
       * {{{
       *       b0
       *       /  \
       *     b1'  b1''
       * }}}
       */
      "there might be scopes with the same name in a tree" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1") // (1) |
        val b2 = SBlock("b1") // (2) | (1), (2) are different scopes that share name, both should exist in the tree

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(b1), EqWrap(b2))
        t1.edges.size mustBe (2)
      }

      "allow Block, Method, Struct to be added" in {
        val b0 = SBlock("globals")
        val m0 = SMethod("main")
        val s0 = SStruct("struct")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(m0, b0)
          .link(s0, b0)

        t1.vertices.size mustBe (3)
        t1.vertices must contain allElementsOf List(EqWrap(b0), EqWrap(m0), EqWrap(s0))
        t1.edges.size mustBe (2)
      }
    }

    "show" should {
      import ScopeTree.*
      import com.github.gchudnov.bscript.lang.util.Show.*

      "display an empty tree" in {
        val t0 = ScopeTree.empty

        val s = t0.show()

        s mustBe ("""{
                     |  "vertices": [],
                     |  "edges": []
                     |}
                     |""".stripMargin.trim)
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
      "display a non-empty tree" in {
        val b0 = SBlock("b0")
        val b1 = SBlock("b1")
        val b2 = SBlock("b2")
        val b3 = SBlock("b3")

        val t1 = ScopeTree.empty
          .add(b0)
          .link(b1, b0)
          .link(b2, b0)
          .link(b3, b2)

        val s = t1.show()

        s mustBe ("""{
                     |  "vertices": ["b0","b1","b2","b3"],
                     |  "edges": [["b1","b0"],["b2","b0"],["b3","b2"]]
                     |}
                     |""".stripMargin.trim)
      }
    }
  }
