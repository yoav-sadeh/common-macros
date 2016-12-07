package com.hamlazot.common.tests.mocks

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

/**
 * @author yoav @since 11/20/16.
 */
case class MockDateTimeContainer(zdt: ZonedDateTime = ZonedDateTime.now.truncatedTo(ChronoUnit.SECONDS))
