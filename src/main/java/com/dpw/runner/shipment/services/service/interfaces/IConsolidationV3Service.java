package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.AibActionConsolidation;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CalculateAchievedValueRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.response.ConsolidationPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentWtVolResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationEtV3Request;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationSailingScheduleRequest;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationSailingScheduleResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.nimbusds.jose.util.Pair;
import org.apache.http.auth.AuthenticationException;
import org.springframework.http.ResponseEntity;

import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface IConsolidationV3Service {
    ShipmentGridChangeV3Response calculateAchievedValues(CalculateAchievedValueRequest request) throws RunnerException;
    ConsolidationDetailsV3Response create(ConsolidationDetailsV3Request request);
    Pair<ConsolidationDetails, Long> createConsolidationForBooking(CommonRequestModel commonRequestModel, CustomerBookingV3Request request);
    ConsolidationDetailsV3Response completeUpdate(ConsolidationDetailsV3Request consolidationDetailsRequest) throws RunnerException;
    void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException;
    String attachShipments(ShipmentConsoleAttachDetachV3Request shipmentAttachDetachRequest) throws RunnerException;

    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails) throws RunnerException;
    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, ConsolidationDetails oldEntity);
    ConsolidationDetails fetchConsolidationDetails(Long consolidationId);
    ConsolidationDetailsV3Response retrieveById(CommonGetRequest commonGetRequest, String source) throws RunnerException, AuthenticationException;
    Map<String, Object> getAllMasterData(Long id, String source) throws RunnerException, AuthenticationException;
    ConsolidationPendingNotificationResponse getPendingNotificationData(CommonGetRequest request);
    ConsolidationListV3Response list(ListCommonRequest listCommonRequest, boolean getMasterData);
    ConsolidationListV3Response getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> detachShipments(@Valid ShipmentConsoleAttachDetachV3Request request)
        throws RunnerException;
    ConsolidationDetails getConsolidationById(Long consolidationId);
    Optional<ConsolidationDetails> findById(Long consolidationId);
    ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync);
    String getBookingNumberFromConsol(Long consolidationId);
    void checkSciForDetachConsole(Long consoleId) throws RunnerException;

    void updateConsolidationAttachmentFlag(Boolean enableFlag, Long consolId);
    VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException;
    ConsolidationSailingScheduleResponse updateSailingScheduleDataToShipment(
        ConsolidationSailingScheduleRequest request) throws RunnerException;

    ShipmentWtVolResponse calculateShipmentWtVol(ConsolidationDetails consolidationDetails) throws RunnerException;
    void updateConsolidationCargoSummary(ConsolidationDetails consolidationDetails, ShipmentWtVolResponse oldShipmentWtVolResponse) throws RunnerException;
    ResponseEntity<IRunnerResponse> aibAction(AibActionConsolidation request) throws RunnerException;
    ResponseEntity<IRunnerResponse> aibPendingNotification(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);
    AchievedQuantitiesResponse getConsoleSyncAchievedData(Long consolidationId) throws RunnerException, JsonMappingException;
    ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationEtV3Request request);
    ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationEtV3Request request) throws RunnerException;
    Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetailsV3Response consolidationDetailsV3Response);
    void triggerPushToDownStream(ConsolidationDetails consolidationDetails, ConsolidationDetails oldConsolidationDetails,
                                 String sourceInfo);
}