package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

public interface INetworkTransferService{
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    void createNetworkTransferEntity(String entityType, ShipmentDetails shipmentDetails, Long tenantId, ConsolidationDetails consolidationDetails);
    ResponseEntity<IRunnerResponse> requestForTransfer(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> requestForReassign(CommonRequestModel commonRequestModel);
}
