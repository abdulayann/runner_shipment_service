package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.TransferredNetworkTransferRequest;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

public interface INetworkTransferService{
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> transferredNetworkTransferStatus(TransferredNetworkTransferRequest request);
    void processNetworkTransferEntity(String entityType, ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails,
                                      Long tenantId, Long oldTenantId, Long entityId);
}
