package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import javax.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;

public interface IConsolidationService extends ICommonService{
    ResponseEntity<IRunnerResponse> fetchConsolidations(CommonRequestModel commonRequestModel);
    CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;
    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;
    ConsolidationDetailsResponse completeUpdateConsolidationFromEntityTransfer(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException;
    ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException;

    ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException;

    void syncMainCarriageRoutingToShipment(List<Routings> consolidationRoutings, ShipmentDetails shipmentDetails, boolean saveRoutes) throws RunnerException;

    ResponseEntity<IRunnerResponse> calculateUtilization(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateAchievedValues(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateChargeable(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> attachShipments(ShipmentRequestedType shipmentRequestedType, Long consolidationId, List<Long> shipmentIds, boolean fromConsolidation) throws RunnerException;

    ResponseEntity<IRunnerResponse> detachShipments(ConsolidationDetails consol, List<Long> shipmentIds) throws RunnerException;

    ResponseEntity<IRunnerResponse> detachShipments(Long consolidationId, List<Long> shipmentIds, String remarks) throws RunnerException;

    ResponseEntity<IRunnerResponse> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel, boolean dataMigration, String createdBy, LocalDateTime createdDate) throws RunnerException;

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException, RunnerException;

    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate, List<Containers> oldContainers);

    void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException;

    void generateEvents(ConsolidationDetails consolidationDetails);

    ResponseEntity<IRunnerResponse> getConsolFromShipment(Long shipmentId);

    ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> calculatePackUtilisation(CommonRequestModel commonRequestModel) throws RunnerException;
    VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException;
    ResponseEntity<IRunnerResponse> listPacksForAssignDetach(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> assignPacksAndShipments(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> detachPacksAndShipments(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel);
    Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse);
    ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getContainerPackSummary(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getDefaultConsolidation();
    ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException;
    ResponseEntity<IRunnerResponse> validateMawbNumber(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel);
    ConsolidationDetailsResponse createConsolidationForBooking(CommonRequestModel commonRequestModel);
    ConsolidationDetailsResponse createConsolidationFromEntityTransfer(ConsolidationDetailsRequest request);
    ResponseEntity<IRunnerResponse> updateConsoleBookingFields(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> showCreateBooking(String operation) throws RunnerException;
    ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> checkContainerEditingRequiredForOceanDg(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> fullConsolidationsList(CommonRequestModel commonRequestModel);
    void checkSciForDetachConsole(Long consoleId) throws RunnerException;
    void checkSciForAttachConsole(Long consoleId) throws RunnerException;
    void validateRaKcForConsol(ConsolidationDetails consolidationDetails, V1TenantSettingsResponse tenantSettingsResponse) throws RunnerException;
    ResponseEntity<IRunnerResponse> consolidationRetrieveWithMeasurmentBasis(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> mblCheck(String mblNumber);
    ResponseEntity<IRunnerResponse> getPendingNotifications(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getDGShipment(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> listRequestedConsolidationForShipment(CommonRequestModel commonRequestModel, boolean getMasterData);

    ResponseEntity<IRunnerResponse> cancel(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel, boolean getMasterData);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean getMasterData);
    ResponseEntity<IRunnerResponse> retrieveForNTE(CommonRequestModel commonRequestModel);
    void triggerAutomaticTransfer(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isDocOrHawbNumAdded);
}