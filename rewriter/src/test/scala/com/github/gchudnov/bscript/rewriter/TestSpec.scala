package com.github.gchudnov.bscript.rewriter

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{ EitherValues, OptionValues }

abstract class TestSpec extends AnyWordSpec with Matchers with OptionValues with EitherValues {}
