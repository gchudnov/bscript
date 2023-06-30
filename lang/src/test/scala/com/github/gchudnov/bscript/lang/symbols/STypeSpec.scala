package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.TestSpec
import org.scalatest.prop.TableDrivenPropertyChecks.*

final class STypeSpec extends TestSpec:

  "SType" when {
    "a primitive type name" should {
      "be parsed" in {
        val t = Table(
          ("input", "expected"),
          ("nothing", Some(SType.nothing)),
          ("void", Some(SType.void)),
          ("bool", Some(SType.bool)),
          ("u8",  Some(SType.u8)),
          ("i16", Some(SType.i16)),
          ("i32", Some(SType.i32)),
          ("i64", Some(SType.i64)),
          ("f32", Some(SType.f32)),
          ("f64", Some(SType.f64)),
          ("dec", Some(SType.dec)),
          ("chr", Some(SType.chr)),
          ("str", Some(SType.str)),
          ("date", Some(SType.date)),
          ("datetime", Some(SType.datetime)),
          ("unknown", None),
        )

        forAll(t) { (input, expected) =>
          val actual = SType.parse(input)
          assert(actual == expected)
        }
      }
    }
  }
