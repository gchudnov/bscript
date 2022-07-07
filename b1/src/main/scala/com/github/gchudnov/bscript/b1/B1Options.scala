package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.translator.Lang

final case class B1Options(
  hasPrelude: Boolean = true,
  lang: Lang = Lang.Scala3
):
  def withPrelude(hasPrelude: Boolean): B1Options = copy(hasPrelude = hasPrelude)

  def withLang(lang: Lang): B1Options = copy(lang = lang)

object B1Options:
  val default: B1Options =
    B1Options()
