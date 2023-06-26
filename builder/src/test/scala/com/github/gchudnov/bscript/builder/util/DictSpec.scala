package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * Dict Tests
 */
final class DictSpec extends TestSpec:

  final case class MyKey(name: String)
  final case class MyVal(value: String)

  object MyKey:
    given Show[MyKey] = new Show[MyKey]:
      override def show(a: MyKey): String =
        s"mykey(${a.name})"

  object MyVal:
    given Show[MyVal] = new Show[MyVal]:
      override def show(a: MyVal): String =
        s"myval(${a.value})"

    given Show[Ptr[MyVal]] = new Show[Ptr[MyVal]]:
      override def show(a: Ptr[MyVal]): String =
        s"ptr(${summon[Show[MyVal]].show(a.value)})"

  final case class MyKeyValDict(keyValues: Map[MyKey, Set[Ptr[MyVal]]], valueKey: Map[Ptr[MyVal], MyKey]) extends Dict[MyKey, Ptr[MyVal], MyKeyValDict]:
    override def clone(keyValues: Map[MyKey, Set[Ptr[MyVal]]], valueKey: Map[Ptr[MyVal], MyKey]): MyKeyValDict =
      MyKeyValDict(keyValues = keyValues, valueKey = valueKey)

    def link(key: MyKey, value: MyVal): MyKeyValDict =
      set(key, Ptr(value))

    def MyVals(key: MyKey): List[MyVal] =
      values(key).map(_.value)

    def myKey(value: MyVal): Option[MyKey] =
      key(Ptr(value))

  object MyKeyValDict:
    val empty: MyKeyValDict = MyKeyValDict(keyValues = Map.empty[MyKey, Set[Ptr[MyVal]]], valueKey = Map.empty[Ptr[MyVal], MyKey])

  "DictSpec" when {

    val k1 = MyKey("a")

    val v1 = MyVal("myFunc1")
    val v2 = MyVal("myFunc1")

    val ss = MyKeyValDict.empty

    "link" should {

      "link value to a key" in {
        val ss1 = ss.link(k1, v1)

        ss1.keyValues must contain theSameElementsAs (List(MyKey("a") -> Set(Ptr(v1))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v1) -> MyKey("a")))
      }

      "allows multiple values for the same key" in {
        val ss1 = ss.link(k1, v1).link(k1, v2)

        ss1.keyValues must contain theSameElementsAs (List(MyKey("a") -> Set(Ptr(v1), Ptr(v2))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v1) -> MyKey("a"), Ptr(v2) -> MyKey("a")))
      }

      "locate values after they were linked" in {
        val ss1 = ss.link(k1, v1).link(k1, v2)

        val actual   = ss1.MyVals(k1)
        val expected = List(v1, v2)

        actual must contain theSameElementsAs (expected)
      }

      "key can be found by value if the value was linked" in {
        val ss1 = ss.link(k1, v1)

        val actual   = ss1.myKey(v1)
        val expected = Some(k1)

        actual mustBe expected
      }

      "key cannot be found by value if the value was not linked" in {
        val actual   = ss.myKey(v1)
        val expected = None

        actual mustBe expected
      }

      "value cannot be linked twice to the same key" in {
        assertThrows[IllegalArgumentException] {
          ss.link(k1, v1).link(k1, v1)
        }
      }
    }

    "show" should {
      "print the dict" in {
        val ss1 = ss.link(k1, v1).link(k1, v2)

        val actual = ss1.show
        val expected = """
                         |{
                         |  "mykey(a)": ["ptr(myval(myFunc1))","ptr(myval(myFunc1))"]
                         |}
                         |""".stripMargin

        actual.trim mustBe expected.trim
      }

      "print the empty dict" in {
        val actual   = ss.show
        val expected = """
                         |{
                         |}
                         |""".stripMargin

        actual.trim mustBe expected.trim
      }
    }
  }
