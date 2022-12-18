package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.util.Ptr


final class DirectorySpec extends TestSpec:

  final case class Key(name: String)
  final case class Value(value: String)

  "DirectorySpec" when {

    "key is added" should {
      val k = Key("a")

      val ss = Directory.empty[Key, Value]
        .addKey(k)

      "contain the newly added key" in {
        ss.keyValues must contain theSameElementsAs (List(Key("a") -> Set.empty[Ptr[Value]]))
        ss.valueKey must contain theSameElementsAs (List.empty)
      }

      "new values can be linked to that key" in {
        val v = Value("myFunc")

        val ss1 = ss.link(k, v)

        ss1.keyValues must contain theSameElementsAs (List(Key("a") -> Set(Ptr(v))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v) -> Key("a")))
      }

      "values might have the same value, but must point to a different entry" in {
        val v1 = Value("myFunc1")
        val v2 = Value("myFunc1")

        val ss1 = ss.link(k, v1).link(k, v2)

        ss1.keyValues must contain theSameElementsAs (List(Key("a") -> Set(Ptr(v1), Ptr(v2))))
        ss1.valueKey must contain theSameElementsAs (List(Ptr(v1) -> Key("a"), Ptr(v2) -> Key("a")))
      }
    }
  }
