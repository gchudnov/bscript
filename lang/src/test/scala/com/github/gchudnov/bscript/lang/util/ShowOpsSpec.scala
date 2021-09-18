package com.github.gchudnov.bscript.lang.util

import com.github.gchudnov.bscript.lang.TestSpec

final class ShowOpsSpec extends TestSpec:

  "ShowOps" when {

    "padLine" should {
      "pad string" in {
        val s        = "ABC"
        val actual   = ShowOps.padLine(3, s)
        val expected = "   ABC"

        actual mustBe expected
      }

      "pad empty string" in {
        val s        = ""
        val actual   = ShowOps.padLine(3, s)
        val expected = "   "

        actual mustBe expected
      }

      "pad string with a pattern" in {
        val p        = " * "
        val s        = "ABC"
        val actual   = ShowOps.padLine(p, s)
        val expected = " * ABC"
        actual mustBe expected
      }
    }

    "padLines" should {
      "pad several lines with a pattern" in {
        val p        = " * "
        val ss       = List("ABC", "DEF")
        val actual   = ShowOps.padLines(p, ss)
        val expected = List(" * ABC", " * DEF")

        actual mustBe expected
      }

      "pad several lines" in {
        val ss       = List("ABC", "DEF")
        val actual   = ShowOps.padLines(2, ss)
        val expected = List("  ABC", "  DEF")

        actual mustBe expected
      }

      "no-op if the list is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.padLines(2, ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "padTail" should {
      "pad the tail of the collection" in {
        val ss       = List("ABC", "DEF")
        val actual   = ShowOps.padTail(2, ss)
        val expected = List("ABC", "  DEF")

        actual mustBe expected
      }

      "no-op if the list is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.padTail(2, ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "tabLine" should {
      "tabulate one line" in {
        val s        = "123"
        val actual   = ShowOps.tabLine(1, s)
        val expected = "  123"

        actual mustBe expected
      }

      "tabulate one line if empty" in {
        val s        = ""
        val actual   = ShowOps.tabLine(1, s)
        val expected = "  "

        actual mustBe expected
      }
    }

    "tabLines" should {
      "tabulate several line" in {
        val s        = List("123", "456")
        val actual   = ShowOps.tabLines(1, s)
        val expected = List("  123", "  456")

        actual mustBe expected
      }

      "no-op if collection is empty" in {
        val s        = List.empty[String]
        val actual   = ShowOps.tabLines(1, s)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "tabText" should {
      "tabulate multiline text" in {
        val s        = "123\n456"
        val actual   = ShowOps.tabText(1, s)
        val expected = "  123\n  456"

        actual mustBe expected
      }

      "tab if text is empty" in {
        val s        = ""
        val actual   = ShowOps.tabText(1, s)
        val expected = "  "

        actual mustBe expected
      }
    }

    "tabTail" should {
      "tabulate tail of the text" in {
        val s        = "123\n456\n789"
        val actual   = ShowOps.tabTail(1, s)
        val expected = "123\n  456\n  789"

        actual mustBe expected
      }

      "no-op if the text if empty" in {
        val s        = ""
        val actual   = ShowOps.tabTail(1, s)
        val expected = ""

        actual mustBe expected
      }

      "tabulate tail of the collection" in {
        val ss       = List("123", "456", "789")
        val actual   = ShowOps.tabTail(1, ss)
        val expected = List("123", "  456", "  789")

        actual mustBe expected
      }

      "no-op if the collection if empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.tabTail(1, ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "prepend" should {
      "prepend a string to the collection" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.prepend("(", ss)
        val expected = List("(123", "456")

        actual mustBe expected
      }

      "no-op if the collection is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.prepend("(", ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "append" should {
      "append a string to the collection" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.append(")", ss)
        val expected = List("123", "456)")

        actual mustBe expected
      }

      "no-op if the collection is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.append(")", ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "wrap" should {
      "wrap a collection in the start and end delimiters" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.wrap("(", ")", ss)
        val expected = List("(123", "456)")

        actual mustBe expected
      }

      "no-op if the collection is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.wrap("(", ")", ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "wrapIfMultiline" should {
      "not wrap one line" in {
        val ss       = List("123")
        val actual   = ShowOps.wrapIfMultiline("(", ")", ss)
        val expected = List("123")

        actual mustBe expected
      }

      "wrap several lines" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.wrapIfMultiline("(", ")", ss)
        val expected = List("(123", "456)")

        actual mustBe expected
      }

      "no-op if list is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.wrapIfMultiline("(", ")", ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "wrapIfNonWrapped" should {
      "wrap if not wrapped" in {
        val ss       = List("123")
        val actual   = ShowOps.wrapIfNonWrapped("(", ")", ss)
        val expected = List("(123)")

        actual mustBe expected
      }

      "not wrap if wrapped" in {
        val ss       = List("(123)")
        val actual   = ShowOps.wrapIfNonWrapped("(", ")", ss)
        val expected = List("(123)")

        actual mustBe expected
      }

      "no-op if the list is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.wrapIfNonWrapped("(", ")", ss)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "wrapEmpty" should {
      "wrap in empty lines" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.wrapEmpty(ss)
        val expected = List("", "123", "456", "")

        actual mustBe expected
      }

      "wrap if collection is empty" in {
        val ss       = List.empty[String]
        val actual   = ShowOps.wrapEmpty(ss)
        val expected = List("", "")

        actual mustBe expected
      }
    }

    "join" should {
      "join lines" in {
        val ss       = List("123", "456")
        val actual   = ShowOps.join(ss)
        val expected = "123\n456"

        actual mustBe expected
      }

      "join left and right arrays" in {
        val lhs      = List("123", "456", "789")
        val rhs      = List("ABC", "DEF")
        val actual   = ShowOps.join(" = ", lhs, rhs)
        val expected = List("123", "456", "789 = ABC", "      DEF")

        actual mustBe expected
      }

      "join left and right arrays if one-liners" in {
        val lhs      = List("123")
        val rhs      = List("ABC")
        val actual   = ShowOps.join(" = ", lhs, rhs)
        val expected = List("123 = ABC")

        actual mustBe expected
      }

      "return rhs if the lhs is empty" in {
        val lhs      = List.empty[String]
        val rhs      = List("ABC")
        val actual   = ShowOps.join(" = ", lhs, rhs)
        val expected = List("ABC")

        actual mustBe expected
      }

      "return lhs if the rhs is empty" in {
        val lhs      = List("DEF")
        val rhs      = List.empty[String]
        val actual   = ShowOps.join(" = ", lhs, rhs)
        val expected = List("DEF")

        actual mustBe expected
      }

      "no-op if the lhs & rhs are empty" in {
        val lhs      = List.empty[String]
        val rhs      = List.empty[String]
        val actual   = ShowOps.join(" = ", lhs, rhs)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "joinAll" should {
      "join one-liners" in {
        val sss      = List(List("123"), List("ABC"))
        val actual   = ShowOps.joinAll(", ", sss)
        val expected = List("123, ABC")

        actual mustBe expected
      }

      "join several arrays" in {
        val sss = List(
          List("123", "456"),
          List("ABC"),
          List("FGH", "JKL")
        )

        val actual   = ShowOps.joinAll(", ", sss)
        val expected = List("123", "456, ABC, FGH", "          JKL")

        actual mustBe expected
      }

      "no-op if the list is empty" in {
        val sss      = List.empty[List[String]]
        val actual   = ShowOps.joinAll(", ", sss)
        val expected = List.empty[List[String]]

        actual mustBe expected
      }

      "join if list of lists contains empty lists" in {
        val sss = List(
          List("123", "456"),
          List.empty[String],
          List("FGH", "JKL")
        )

        val actual   = ShowOps.joinAll(", ", sss)
        val expected = List("123", "456, FGH", "     JKL")

        actual mustBe expected
      }
    }

    "joinCR" should {
      "join arrays with overlap" in {
        val lhs      = List("123", "456")
        val rhs      = List("ABC", "DEF")
        val actual   = ShowOps.joinCR(" = ", lhs, rhs)
        val expected = List("123", "456 = ABC", "DEF")

        actual mustBe expected
      }

      "join one-liners with overlap" in {
        val lhs      = List("123")
        val rhs      = List("ABC")
        val actual   = ShowOps.joinCR(" = ", lhs, rhs)
        val expected = List("123 = ABC")

        actual mustBe expected
      }

      "return rhs if lhs is empty" in {
        val lhs      = List.empty[String]
        val rhs      = List("ABC")
        val actual   = ShowOps.joinCR(" = ", lhs, rhs)
        val expected = List("ABC")

        actual mustBe expected
      }

      "return lhs if rhs is empty" in {
        val lhs      = List("123")
        val rhs      = List.empty[String]
        val actual   = ShowOps.joinCR(" = ", lhs, rhs)
        val expected = List("123")

        actual mustBe expected
      }

      "return empty if lhs & rhs are empty" in {
        val lhs      = List.empty[String]
        val rhs      = List.empty[String]
        val actual   = ShowOps.joinCR(" = ", lhs, rhs)
        val expected = List.empty[String]

        actual mustBe expected
      }
    }

    "joinVAll" should {
      "join arrays vertically" in {
        val sss = List(
          List("123", "456"),
          List("ABC", "DEF")
        )
        val actual   = ShowOps.joinVAll(",", sss)
        val expected = List("123", "456,", "ABC", "DEF")

        actual mustBe expected
      }

      "join arrays vertically of one of them is empty" in {
        val sss = List(
          List("123", "456"),
          List.empty[String],
          List("ABC", "DEF")
        )
        val actual   = ShowOps.joinVAll(",", sss)
        val expected = List("123", "456,", "ABC", "DEF")

        actual mustBe expected
      }
    }

    "quote" should {
      "quote a string" in {
        val s        = "ABC"
        val actual   = ShowOps.quote(s)
        val expected = """"ABC""""

        actual mustBe expected
      }

      "quote a string if the input is empty" in {
        val s        = ""
        val actual   = ShowOps.quote(s)
        val expected = """"""""

        actual mustBe expected
      }
    }
  }
