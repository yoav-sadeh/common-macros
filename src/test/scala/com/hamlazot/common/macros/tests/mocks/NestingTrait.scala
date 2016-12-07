package com.hamlazot.common.tests.mocks

/**
 * @author yoav @since 11/26/16.
 */
trait NestingTrait {
  type Trustees
  case class NestedCaseClassWithShitInIt(str: String, i: BigInt, trustees: Trustees)

  case class NestedCaseClassWithShitInItWithSnakes(str: String, i: BigInt, theSnake: Trustees)

  case class NestedCaseClass(str: String, i: BigInt)
}
