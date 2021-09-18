package com.github.gchudnov.bscript.lang.util

import com.github.gchudnov.bscript.lang.TestSpec

final class BaseNSpec extends TestSpec:

  object Base2 extends BaseN:
    override def base: Long = 2

    override def lookup(x: Long): Char = x match
      case 0 => 'f';
      case _ => 't'

    override def reverseLookup(c: Char): Long = c match
      case 'f' => 0L;
      case _   => 1L

  "BaseN" when {

    "base2" should {
      "encode" in {
        Base2.encode(0L) mustBe ("f")
        Base2.encode(1L) mustBe ("t")
        Base2.encode(2L) mustBe ("tf")
        Base2.encode(3L) mustBe ("tt")
        Base2.encode(4L) mustBe ("tff")
        Base2.encode(5L) mustBe ("tft")
        Base2.encode(6L) mustBe ("ttf")
        Base2.encode(7L) mustBe ("ttt")
        Base2.encode(8L) mustBe ("tfff")
      }

      "decode" in {
        Base2.decode("f") mustBe (0L)
        Base2.decode("t") mustBe (1L)
        Base2.decode("tf") mustBe (2L)
        Base2.decode("tt") mustBe (3L)
        Base2.decode("tff") mustBe (4L)
        Base2.decode("tft") mustBe (5L)
        Base2.decode("ttf") mustBe (6L)
        Base2.decode("ttt") mustBe (7L)
        Base2.decode("tfff") mustBe (8L)
      }
    }

    "base26" should {
      "encode" in {
        Base26.encode(0L) mustBe ("a")
        Base26.encode(25L) mustBe ("z")
        Base26.encode(26L) mustBe ("ba")
      }

      "decode" in {
        Base26.decode("a") mustBe (0L)
        Base26.decode("z") mustBe (25L)
        Base26.decode("ba") mustBe (26L)
      }
    }
  }
