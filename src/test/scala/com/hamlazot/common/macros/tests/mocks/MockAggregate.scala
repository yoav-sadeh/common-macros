package com.hamlazot.common.macros.tests.mocks

import java.time.ZonedDateTime
import java.util.UUID
/**
 * @author yoav @since 12/7/16.
 */
trait MockAggregate extends MockAbstractAggregate{
  override type Mock = MockModel
  override type Reference2 = Map[UUID, Int]
  override type Reference1 = ZonedDateTime
  override type MockId = UUID
}
