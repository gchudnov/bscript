package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.lang.util.LineOps.split

object Inits:

  def code: Seq[String] =
    split(
      s"""// Allow LocalDate ordering: <, <=, >, >=
         |given toOrderedLocalDate: Conversion[LocalDate, Ordered[LocalDate]] with
         |  def apply(x: LocalDate): Ordered[LocalDate] = (y: LocalDate) => x.compareTo(y)
         |""".stripMargin
    )
