package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import org.apache.http.auth.AuthenticationException;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface IShipmentServiceV3 {

    List<ShipmentDetailsProjection> findShipmentDetailsByAttachedContainerIds(List<Long> containerIds);

    NotificationCount getPendingNotificationCount();

    ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ShipmentPacksAssignContainerTrayDto getShipmentAndPacksForConsolidationAssignContainerTray(Long containerId, Long consolidationId);
    ShipmentPacksUnAssignContainerTrayDto getShipmentAndPacksForConsolidationUnAssignContainerTray(Long containerId);

    ShipmentDetailsV3Response create(CommonRequestModel commonRequestModel);

    void delete(CommonRequestModel commonRequestModel);

    ShipmentRetrieveLiteResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData, String source) throws RunnerException, AuthenticationException;

    ShipmentDetailsV3Response completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;

    void createLogHistoryForShipment(ShipmentDetails shipmentDetails);

    void syncShipmentsList(List<ShipmentDetails> shipments, String transactionId);

    List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException;

    ShipmentPendingNotificationResponse getPendingNotificationData(CommonGetRequest request);

    Optional<ShipmentDetails> findById(Long shipmentId);

    void updateCargoDetailsInShipment(Long shipmentId, CargoDetailsResponse cargoDetailsResponse);

    void updateShipmentDetailsFromPacks(Long shipmentId, DateBehaviorType dateType, LocalDateTime shipmentGateInDate, ShipmentPackStatus shipmentPackStatus);

    String attachConsolidation(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    Map<String, Object>  getAllMasterData(Long shipmentId, String xSource);

    ShipmentDetailsV3Response createShipmentInV3(CustomerBookingV3Request customerBookingRequest) throws RunnerException;

    List<RoutingsRequest> getCustomerBookingRequestRoutingList(CarrierDetailRequest carrierDetailRequest, String transportMode);
    void updateShipmentFieldsAfterDetach(List<ShipmentDetails> detachedShipments);

    ShipmentSailingScheduleResponse updateSailingScheduleDataToShipment(ShipmentSailingScheduleRequest request) throws RunnerException;
    void updateCutoffDetailsToShipment(ShipmentSailingScheduleRequest request, ShipmentDetails shipmentDetails);
    Long assignFirstBookingContainerToShipmentCargo(List<Containers> expandedContainers, CustomerBookingV3Request customerBookingV3Request) throws RunnerException;
}