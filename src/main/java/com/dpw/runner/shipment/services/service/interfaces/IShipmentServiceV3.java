package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.Optional;

public interface IShipmentServiceV3 {

    NotificationCount getPendingNotificationCount();

    ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ShipmentPacksAssignContainerTrayDto getShipmentAndPacksForConsolidationAssignContainerTray(Long containerId, Long consolidationId);

    ShipmentDetailsV3Response create(CommonRequestModel commonRequestModel);

    void delete(CommonRequestModel commonRequestModel);

    ShipmentRetrieveLiteResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException;

    ShipmentDetailsV3Response completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;

    ShipmentPendingNotificationResponse getPendingNotificationData(CommonGetRequest request);

    Optional<ShipmentDetails> findById(Long shipmentId);

    void updateCargoDetailsInShipment(Long shipmentId, CargoDetailsResponse cargoDetailsResponse);

    String attachConsolidation(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;
}