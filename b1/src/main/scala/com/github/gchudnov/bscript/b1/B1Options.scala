package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.translator.Lang

/**
 * Options for B1
 */
final case class B1Options(
  hasPrelude: Boolean,
  lang: Lang
):
  def withPrelude(hasPrelude: Boolean): B1Options = copy(hasPrelude = hasPrelude)

  def withLang(lang: Lang): B1Options = copy(lang = lang)

object B1Options:
  val default: B1Options =
    B1Options(
      hasPrelude = true,
      lang = Lang.Scala3
    )
