package com.github.gchudnov.bscript.translator

import org.scalatest.{ EitherValues, OptionValues }
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class TestSpec extends AnyWordSpec with Matchers with OptionValues with EitherValues {}
