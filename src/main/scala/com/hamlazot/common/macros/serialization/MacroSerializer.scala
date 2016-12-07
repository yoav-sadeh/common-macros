package com.hamlazot.common.macros.serialization

import java.util.UUID

import com.hamlazot.common.macros.Macros.{Mapper, Serializer}
import com.hamlazot.common.serialization.JsonSerializer


/**
 * @author yoav @since 12/6/16.
 */
trait MacroSerializer extends JsonSerializer with Mappable {

  def marshal[A: Serializer](json: String): A = {
    val deserializer = implicitly[Serializer[A]]
    deserializer.formats = Some(this.jsonFormat)
    deserializer.deserializ(json)
  }

  def seriamap[A: Mapper](a: A): String = {
    serialize(map[A](a))
  }

//  def deseriamap[A](json: String)(implicit mapp: Mapper[A]): A = {
//    val parsed = parse(json)
//
//    deMap[A](parsed.map(transformDeserialized).values.asInstanceOf[Map[String, Any]])
//  }


}
