package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.util.Ptr

final class DictSpec extends TestSpec:

  final case class Idx(name: String)
  final case class Val(value: String)

  final case class IdxValDict(keyValues: Map[Idx, Set[Ptr[Val]]], valueKey: Map[Ptr[Val], Idx]) extends Dict[Idx, Ptr[Val], IdxValDict]:
    override def clone(keyValues: Map[Idx, Set[Ptr[Val]]], valueKey: Map[Ptr[Val], Idx]): IdxValDict =
      IdxValDict(keyValues = keyValues, valueKey = valueKey)

    def addIdx(idx: Idx): IdxValDict =
      addKey(idx)

    def link(idx: Idx, value: Val): IdxValDict =
      set(idx, Ptr(value))

    def indices(idx: Idx): List[Val] =
      values(idx).map(_.value)

    def idx(value: Val): Option[Idx] =
      key(Ptr(value))

  object IdxValDict:
    val empty: IdxValDict = IdxValDict(keyValues = Map.empty[Idx, Set[Ptr[Val]]], valueKey = Map.empty[Ptr[Val], Idx])

  "DictSpec" when {

    "key is added" should {
      val k = Idx("a")

      val ss = IdxValDict.empty.addIdx(k)

      "contain the newly added key" in {
        ss.keyValues must contain theSameElementsAs (List(Idx("a") -> Set.empty[Ptr[Val]]))
        ss.valueKey must contain theSameElementsAs (List.empty)
      }

      "new values can be linked to that key" in {
        val v = Val("myFunc")

        val ss1 = ss.link(k, v)

        ss1.keyValues must contain theSameElementsAs (List(Idx("a") -> Set(Ptr(v))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v) -> Idx("a")))
      }

      "values might have the same value, but must point to a different entry" in {
        val v1 = Val("myFunc1")
        val v2 = Val("myFunc1")

        val ss1 = ss.link(k, v1).link(k, v2)

        ss1.keyValues must contain theSameElementsAs (List(Idx("a") -> Set(Ptr(v1), Ptr(v2))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v1) -> Idx("a"), Ptr(v2) -> Idx("a")))
      }

      "values can be found" in {
        val v1 = Val("myFunc1")
        val v2 = Val("myFunc1")

        val ss1 = ss.link(k, v1).link(k, v2)

        val actual   = ss1.indices(k)
        val expected = List(v1, v2)

        actual must contain theSameElementsAs (expected)
      }

      "key can be found by value if the value was linked" in {
        val v1 = Val("myFunc1")

        val ss1 = ss.link(k, v1)

        val actual   = ss1.idx(v1)
        val expected = Some(k)

        actual mustBe expected
      }

      "key cannot be found by value if the value was not linked" in {
        val v1 = Val("myFunc1")

        val actual   = ss.idx(v1)
        val expected = None

        actual mustBe expected
      }
    }
  }
