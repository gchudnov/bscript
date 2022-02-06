package com.github.gchudnov.bscript.lang.util

object ShowOps:

  private val NL: String    = System.lineSeparator
  private val SPACE: String = " "

  private val tabSize = 2 // how many spaces is one tab

  /**
   * Pad s string with the given prefix
   * {{{
   * example:
   *   " * "
   *   ,
   *   "ABC"
   * output:
   *   " * ABC"
   * }}}
   */
  def padLine(p: String, str: String): String =
    p + str

  /**
   * Pad string with N spaces
   * {{{
   * example:
   *   2,
   *   "ABC"
   * output:
   *   "  ABC"
   * }}}
   */
  def padLine(n: Int, str: String): String =
    padLine(SPACE.repeat(n), str)

  /**
   * Pad several lines with the given prefix
   *
   * {{{
   * example:
   *   " * "
   *   ,
   *   "ABC"
   *   "DEF"
   * output:
   *   " * ABC"
   *   " * DEF"
   * }}}
   */
  def padLines(p: String, lines: Seq[String]): Seq[String] =
    lines.map(line => padLine(p, line))

  /**
   * Pad several lines
   *
   * {{{
   * example:
   *   2
   *   ,
   *   "ABC"
   *   "DEF"
   * output:
   *   "  ABC"
   *   "  DEF"
   * }}}
   */
  def padLines(n: Int, lines: Seq[String]): Seq[String] =
    lines.map(line => padLine(n, line))

  /**
   * Pad tail of the array with N spaces
   */
  def padTail(n: Int, lines: Seq[String]): Seq[String] =
    if lines.isEmpty then Seq.empty[String]
    else
      val (head, tail) = (lines.head, lines.tail)
      val paddedTail   = padLines(n, tail)
      head +: paddedTail

  /**
   * Tabulate one line
   */
  def tabLine(depth: Int, line: String): String =
    padLine(tabToPadSize(depth), line)

  /**
   * Tab several lines
   */
  def tabLines(depth: Int, lines: Seq[String]): Seq[String] =
    lines.map(line => tabLine(depth, line))

  /**
   * Tabulates text
   * {{{
   * example:
   *   2
   *   "123\n456"
   * output:
   *   "  123\n  456"
   * }}}
   */
  def tabText(depth: Int, text: String): String =
    tabLines(depth, split(text)).mkString(NL)

  /**
   * Tabulates the tail part of the text
   * {{{
   * example:
   *   2
   *   "123\n456"
   * output:
   *   "123\n  456"
   * }}}
   */
  def tabTail(depth: Int, text: String): String =
    tabTail(depth, split(text)).mkString(NL)

  /**
   * Tabulates the tail of the collection
   * {{{
   * example:
   *   2
   *   "123"
   *   "456"
   * output:
   *   "123"
   *   "  456"
   * }}}
   */
  def tabTail(depth: Int, lines: Seq[String]): Seq[String] =
    padTail(tabToPadSize(depth), lines)

  /**
   * Prepend the given text to the first line of collection
   * {{{
   * example:
   *   '(',
   *   111
   *   222
   * output:
   *   (111
   *   222
   * }}}
   */
  def prepend(start: String, lines: Seq[String]): Seq[String] =
    if lines.isEmpty then Seq.empty[String]
    else (start + lines.head) +: lines.tail

  /**
   * Append the given text to the last line of the collection
   * {{{
   * example:
   *   ')'
   *   111
   *   222
   * output:
   *   111
   *   222)
   * }}}
   */
  def append(end: String, lines: Seq[String]): Seq[String] =
    if lines.isEmpty then Seq.empty[String]
    else lines.init :+ (lines.last + end)

  /**
   * Wraps the head and last element of the collection
   * {{{
   * example:
   *   '(', ')'
   *   123
   *   456
   * output:
   *   (123
   *   456)
   * }}}
   */
  def wrap(start: String, end: String, lines: Seq[String]): Seq[String] =
    append(end, prepend(start, lines))

  /**
   * Wraps the collection only if it is multiline
   */
  def wrapIfMultiline(start: String, end: String, lines: Seq[String]): Seq[String] =
    if lines.size > 1 then wrap(start, end, lines)
    else lines

  /**
   * Wrap if not already wrapped
   * {{{
   * example:
   *   '(', ')'
   *   (123
   *   456)
   * output:
   *   (123
   *   456)
   *
   * ^ already wrapped, do nothing
   * }}}
   */
  def wrapIfNonWrapped(start: String, end: String, lines: Seq[String]): Seq[String] =
    if lines.nonEmpty && lines.head.nonEmpty && !lines.head.startsWith(start) && lines.last.nonEmpty && !lines.last.endsWith(end) then wrap(start, end, lines)
    else lines

  /**
   * Wrap in empty lines
   * {{{
   * example:
   *   'struct X {', '}'
   *   "123"
   *   "456"
   * output:
   *   ""
   *   "123"
   *   "456"
   *   ""
   * }}}
   */
  def wrapEmpty(lines: Seq[String]): Seq[String] =
    "" +: lines :+ ""

  /**
   * Joins collection with a line separator
   */
  def join(lines: Seq[String]): String =
    lines.mkString(NL)

  /**
   * Joins left lines with right arrays using the given separator
   * {{{
   * example:
   *   ' + '
   *   123
   *   456
   *   ,
   *   789
   *   zxy
   * output:
   *   123
   *   456 + 789
   *         zxy
   * }}}
   */
  def join(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    if lhs.isEmpty then rhs
    else if rhs.isEmpty then lhs
    else
      val lhsInit = lhs.init
      val joined  = lhs.last + sep + rhs.head
      val rhsTail = padLines((lhs.last + sep).length, rhs.tail)

      (lhsInit :+ joined) ++ rhsTail

  /**
   * Joins several arrays using the given separator
   * {{{
   * example:
   *   ', '
   *   123
   *   456
   *   ,
   *   789
   *   zxy
   *   ,
   *   ABC
   *   DEF
   * output:
   *   123
   *   456, 789
   *        zxy, ABC
   *             DEF
   * }}}
   */
  def joinAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    val nonEmpties = linesLines.filter(_.nonEmpty)
    if nonEmpties.isEmpty then Seq.empty[String]
    else
      nonEmpties.reduceLeft[Seq[String]] { case (acc, lines) =>
        join(sep, acc, lines)
      }

  /**
   * Join vertically with carriage return
   * {{{
   * example:
   *   ','
   *   123
   *   456
   *   ,
   *   ABC
   *   DEF
   * output:
   *   123
   *   456, ABC
   *   DEF
   * }}}
   */
  def joinCR(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    if lhs.isEmpty then rhs
    else if rhs.isEmpty then lhs
    else
      val lhsInit = lhs.init
      val joined  = lhs.last + sep + rhs.head
      (lhsInit :+ joined) ++ rhs.tail

  /**
   * Vertical Join
   * {{{
   * example:
   *   ','
   *   123
   *   456
   *   ,
   *   ABC
   *   DEF
   * output:
   *   123
   *   456,
   *   ABC
   *   DEF
   * }}}
   */
  def joinVAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    val nonEmpties = linesLines.filter(_.nonEmpty)
    if nonEmpties.isEmpty then Seq.empty[String]
    else
      nonEmpties.reduceLeft[Seq[String]] { case (acc, lines) =>
        append(sep, acc) ++ lines
      }

  /**
   * Puts a string in quotes
   */
  def quote(s: String): String =
    s"\"${s}\""

  /**
   * Splits a string into lines
   */
  def split(s: String): Seq[String] =
    s.split(NL).toIndexedSeq

  private def tabToPadSize(n: Int): Int =
    tabSize * n
