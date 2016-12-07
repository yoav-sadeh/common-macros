package com.hamlazot.common.tests.mocks

/**
 * @author yoav @since 11/20/16.
 */
case class CaseClassWithCustomMapKey[A](name: String, map: Map[A, Int])