package com.hamlazot.common.macros.tests.specs

import java.util.UUID

import com.hamlazot.common.macros.Macros.{Mapper, Serializer}
import com.hamlazot.common.macros.serialization.MacroSerializer
import com.hamlazot.common.macros.tests.mocks.CaseClassWithInt
import com.hamlazot.common.serialization.{SnakecaseTransformerUUIDSupport, CamelcaseDeseiralizationTransformer, SnakecaseSerializationTransformer, JsonSerializer}
import com.hamlazot.common.tests.mocks.NestingTrait
import org.json4s.{JsonAST, JField, JValue, JObject}
import org.specs2.mutable.Specification


/**
 * @author yoav @since 2/20/16.
 */
class MacroSerializerSpec
  extends Specification
  with MacroSerializer
  with SnakecaseTransformerUUIDSupport
  with CamelcaseDeseiralizationTransformer{


  "Serializer macro " should {

    "deserialize nested case classes" in {
      object nest extends NestingTrait
      import com.hamlazot.common.macros.Macros.Serializer
      val castable = implicitly[Serializer[nest.NestedCaseClass]]

      val nestedCaseClass = nest.NestedCaseClass("Jojo", 35)
      val serialized = serialize(nestedCaseClass)

      val nested = castable.deserializ(serialized)
      nested shouldEqual nestedCaseClass

    }

    "deserialize nested case classes in a generic manner" in {

      object nest extends NestingTrait
      val nestedCaseClass = nest.NestedCaseClass("Jojo", 35)
      val serialized = serialize(nestedCaseClass)

      val nested = marshal[nest.NestedCaseClass](serialized)
      nested shouldEqual nestedCaseClass
      true shouldEqual (true)
    }

    "deserialize nested case classes with abstract shit in it in a generic manner" in {
      object impl extends NestingTrait {
        type Trustees = String //Map[UUID, Int]
      }

      val nestedCaseClass = impl.NestedCaseClassWithShitInIt("Jojo", 35, UUID.randomUUID.toString)
      impl.NestedCaseClassWithShitInIt.unapply(nestedCaseClass)
      val serialized = serialize(impl.NestedCaseClassWithShitInIt.unapply(nestedCaseClass))
      val json = parse(serialized).asInstanceOf[JObject].children

      val nested = marshal[impl.NestedCaseClassWithShitInIt](serialized)
      nested shouldEqual nestedCaseClass
    }

    "serialize nested case classes with abstract shit in it in a generic manner" in {
      object impl extends NestingTrait {
        type Trustees = CaseClassWithInt
      }

      val nestedCaseClass = impl.NestedCaseClassWithShitInIt("Jojo", 35, CaseClassWithInt(47))
      val serialized = seriamap(nestedCaseClass) //serialize(mappable.toMap(nestedCaseClass))

      serialized.replace(" ", "") shouldEqual s"""{"str":"Jojo","i":35,"trustees":{"the_int":47}}"""

    }

    "serialize nested case classes with abstract string in it in a generic manner" in {
      object impl extends NestingTrait {
        type Trustees = String
      }

      val nestedCaseClass = impl.NestedCaseClassWithShitInIt("Jojo", 35, "Hello")
      val serialized = seriamap(nestedCaseClass) //serialize(mappable.toMap(nestedCaseClass))

      serialized.replace(" ", "") shouldEqual s"""{"str":"Jojo","i":35,"trustees":"Hello"}"""

    }

    "deserialize nested case classes with abstract string in it with snake case keys" in {
      object impl extends NestingTrait {
        type Trustees = String
      }

      val nestedCaseClass = impl.NestedCaseClassWithShitInItWithSnakes("Jojo", 35, "Hello")
      val serialized = seriamap(nestedCaseClass) //serialize(mappable.toMap(nestedCaseClass))

      serialized.replace(" ", "") shouldEqual s"""{"str":"Jojo","i":35,"the_snake":"Hello"}"""

      val deserialized = marshal[impl.NestedCaseClassWithShitInItWithSnakes](serialized)

      deserialized shouldEqual nestedCaseClass
    }

    "deserialize nested case classes with abstract string in it with snake case" in {
      object impl extends NestingTrait {
        type Trustees = Map[UUID, Int]
      }

//      val siri = implicitly[Serializer[impl.NestedCaseClassWithShitInIt]]

     registerCustomKeySerializer[UUID](uuid => uuid.toString, str => UUID.fromString(str))

      import com.hamlazot.common.macros.Macros.Mapper
      val id = UUID.randomUUID()
      val nestedCaseClass = impl.NestedCaseClassWithShitInIt("Jojo", 35, Map(id -> 4))
      val serialized = seriamap(nestedCaseClass)

      serialized.replace(" ", "") shouldEqual s"""{"str":"Jojo","i":35,"trustees":{\"${id.toString}\":4}}"""

      val deserialized = marshal[impl.NestedCaseClassWithShitInIt](serialized)

      deserialized shouldEqual nestedCaseClass
    }


    "deserialize nested case classes with abstract nested case class in a generic manner" in {
      object impl extends NestingTrait {
        type Trustees = NestedCaseClass
      }

      val nestedCaseClass = impl.NestedCaseClassWithShitInIt("Jojo", 35, impl.NestedCaseClass("Hi", 83))
      impl.NestedCaseClassWithShitInIt.unapply(nestedCaseClass)
      val serialized = serialize(impl.NestedCaseClassWithShitInIt.unapply(nestedCaseClass))
      val json = parse(serialized).asInstanceOf[JObject].children
      implicit val serializer: Serializer[impl.NestedCaseClassWithShitInIt] = make[Serializer[impl.NestedCaseClassWithShitInIt]]
      val nested = marshal[impl.NestedCaseClassWithShitInIt](serialized)
      nested shouldEqual nestedCaseClass
    }.pendingUntilFixed("issue123")
  }

  def make[A]: A = ???
}



