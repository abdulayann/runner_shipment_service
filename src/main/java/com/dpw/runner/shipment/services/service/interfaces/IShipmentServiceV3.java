package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import org.springframework.http.ResponseEntity;

public interface IShipmentServiceV3 {

    ResponseEntity<IRunnerResponse> getPendingNotificationCount();

    ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ShipmentPacksAssignContainerTrayDto getShipmentAndPacksForConsolidationAssignContainerTray(Long containerId, Long consolidationId);
}