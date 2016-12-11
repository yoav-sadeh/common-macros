package com.hamlazot.common.macros.tests.mocks

/**
 * @author yoav @since 12/8/16.
 */

trait AbstractMockService extends MockProtocol with MockAbstractAggregate{

  type Operation[A,B]

  def createUser: Operation[CreateMockRequest, CreateMockResponse]

  def getUser: Operation[GetMockRequest, GetMockResponse]

  def deleteUser: Operation[DeleteMockRequest, DeleteMockResponse]

  def addTrustees: Operation[AddReference2Request, AddReference2Response]

  def addTrusters: Operation[AddReference1Request, AddReference1Response]

  def removeTrustees: Operation[RemoveReference2Request, RemoveReference2Response]

  def removeTrusters: Operation[RemoveReference1Request, RemoveReference1Response]

}
