package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.AibActionShipment;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGApprovalRequest;
import com.dpw.runner.shipment.services.dto.request.ocean_dg.OceanDGRequestV3;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.NotificationCount;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentRetrieveLiteResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentPacksUnAssignContainerTrayDto;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
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
import java.util.Set;

public interface IShipmentServiceV3 {

    List<ShipmentDetailsProjection> findShipmentDetailsByAttachedContainerIds(List<Long> containerIds);

    List<ShipmentDetails> getShipmentsFromId(List<Long> shipmentIds);

    NotificationCount getPendingNotificationCount();

    ResponseEntity<IRunnerResponse> listShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ShipmentPacksAssignContainerTrayDto getShipmentAndPacksForConsolidationAssignContainerTray(Long containerId, Long consolidationId);
    ShipmentPacksUnAssignContainerTrayDto getShipmentAndPacksForConsolidationUnAssignContainerTray(Long containerId);

    ShipmentDetailsV3Response create(CommonRequestModel commonRequestModel);

    void delete(CommonRequestModel commonRequestModel);

    ShipmentRetrieveLiteResponse retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData, String source) throws RunnerException, AuthenticationException;

    ShipmentDetailsV3Response completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;

    void createLogHistoryForShipment(ShipmentDetails shipmentDetails);

    List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException;

    ShipmentPendingNotificationResponse getPendingNotificationData(CommonGetRequest request);

    Optional<ShipmentDetails> findById(Long shipmentId);

    void updateCargoDetailsInShipment(ShipmentDetails shipmentDetails, CargoDetailsResponse cargoDetailsResponse);

    void updateShipmentDetailsFromPacks(Long shipmentId, DateBehaviorType dateType, LocalDateTime shipmentGateInDate, ShipmentPackStatus shipmentPackStatus);

    String attachConsolidation(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    Map<String, Object>  getAllMasterData(Long shipmentId, String xSource);

    ShipmentDetailsV3Response createShipmentInV3(CustomerBookingV3Request customerBookingRequest) throws RunnerException;

    List<RoutingsRequest> getCustomerBookingRequestRoutingList(CarrierDetailRequest carrierDetailRequest, String transportMode);
    void updateShipmentFieldsAfterDetach(List<ShipmentDetails> detachedShipments);

    ShipmentSailingScheduleResponse updateSailingScheduleDataToShipment(ShipmentSailingScheduleRequest request) throws RunnerException;
    void updateCutoffDetailsToShipment(ShipmentSailingScheduleRequest request, ShipmentDetails shipmentDetails);
    Long assignFirstBookingContainerToShipmentCargo(List<Containers> expandedContainers, CustomerBookingV3Request customerBookingV3Request) throws RunnerException;

    ShipmentDetailsResponse createShipmentFromEntityTransfer(ShipmentEtV3Request shipmentRequest, boolean includeGuid);

    ShipmentDetailsResponse completeUpdateShipmentFromEntityTransfer(ShipmentEtV3Request shipmentRequest) throws RunnerException;

    Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse);
    void triggerPushToDownStream(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment, Boolean isCreate);

    List<ShipmentDetails> findByIdIn(List<Long> shipmentIds);
    Optional<ShipmentDetails> retrieveForNte(CommonGetRequest request) throws RunnerException, AuthenticationException;

    ResponseEntity<IRunnerResponse> consoleShipmentList(CommonRequestModel commonRequestModel, Long consoleId, String consoleGuid, boolean isAttached, boolean getMasterData,
            boolean fromNte) throws AuthenticationException;

    ResponseEntity<IRunnerResponse> aibAction(AibActionShipment request) throws RunnerException;
    ResponseEntity<IRunnerResponse> aibPushRequest(Long shipId, Long consoleId, String remarks) throws RunnerException;
    ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel, boolean getMasterData);
    ResponseEntity<IRunnerResponse> aibPendingNotification(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);

    String sendOceanDGApprovalEmail(OceanDGApprovalRequest dgApprovalRequest) throws RunnerException;
    String dgApprovalResponse(OceanDGRequestV3 request) throws RunnerException;
    CargoDetailsResponse calculateShipmentSummary(String transportMode, List<Packing> packingList, Set<Containers> containers) throws RunnerException;
    void calculateAndUpdateShipmentCargoSummary(ShipmentDetails shipmentDetails) throws RunnerException;
    void calculateAndUpdateShipmentCargoSummary(ShipmentDetails shipmentDetails, List<Containers> containersList) throws RunnerException;
}