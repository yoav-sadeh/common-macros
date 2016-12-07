package com.hamlazot.common.macros.serialization

import com.hamlazot.common.macros.Macros.Mapper

/**
 * @author yoav @since 12/6/16.
 */
trait Mappable {
//  def deMap[T: Mapper](map: Map[String, Any]) =
//    implicitly[Mapper[T]].fromMap(map)

  def map[T: Mapper](t: T) = implicitly[Mapper[T]].toMap(t)
}
