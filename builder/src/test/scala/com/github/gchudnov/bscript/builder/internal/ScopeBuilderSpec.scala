package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.Meta

final class ScopeBuilderSpec extends TestSpec:

  "ScopeBuilderSpec" when {
    "no input was specified" should {
      "return an empty object" in {
        val sb = ScopeBuilder.make()

        val actual = sb.result
        val expected = Meta.empty

        actual mustBe expected
      }
    }

    "a scope was pushed" should {
      "return state with one scope" in {
        val sb = ScopeBuilder.make()

        
      }
    }
  }