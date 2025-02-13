package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Map;

public interface INetworkTransferService {
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);

    void processNetworkTransferEntity(Long tenantId, Long oldTenantId, String entityType,
                                      ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                      String jobType, Map<String, Object> entityPayload, Boolean isInterBranchEntity);

    void deleteValidNetworkTransferEntity(Long tenantId, Long entityId, String entityType);

    void deleteNetworkTransferEntity(NetworkTransfer networkTransfer);

    void updateNetworkTransferTransferred(NetworkTransfer networkTransfer, Map<String, Object> payload);

    ResponseEntity<IRunnerResponse> requestForTransfer(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> requestForReassign(CommonRequestModel commonRequestModel);

    void updateStatusAndCreatedEntityId(Long id, String status, Long createdEntityId);

    void bulkProcessInterConsoleNte(List<ShipmentDetails> shipmentDetails);

    ResponseEntity<IRunnerResponse> fetchEntityStatus(CommonGetRequest commonGetRequest);
}
