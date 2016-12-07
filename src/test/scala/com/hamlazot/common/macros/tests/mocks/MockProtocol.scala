package com.hamlazot.common.macros.tests.mocks

import java.time.ZonedDateTime
import java.util.UUID
/**
 * @author yoav @since 12/7/16.
 */
trait MockProtocol extends MockAbstractAggregate{ //TODO: test mock protocol serialization with scalacheck

    case class CreateMockRequest(name: String, reference1: Reference1)
    case class CreateMockResponse(mockId: MockId)
    case class GetMockRequest(mockId: MockId)
    case class GetMockResponse(mock: Mock)
    case class DeleteMockRequest(mockId: MockId)
    case class DeleteMockResponse(success: Boolean)
    case class AddReference2Request(mockId: MockId, reference2: Reference2)
    case class AddReference2Response(success: Boolean)
    case class AddReference1Request(mockId: MockId, reference1: Reference1)
    case class AddReference1Response(success: Boolean)
    case class RemoveReference2Request(mockId: MockId, reference2: Reference2)
    case class RemoveReference2Response(success: Boolean)
    case class RemoveReference1Request(mockId: MockId, reference1: Reference1)
    case class RemoveReference1Response(success: Boolean)
}









