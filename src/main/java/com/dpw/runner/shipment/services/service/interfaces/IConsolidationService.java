package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface IConsolidationService extends ICommonService{
    List<ConsolidationDetails> createTestConsolidations(Integer count);
    ResponseEntity<IRunnerResponse> fetchConsolidations(CommonRequestModel commonRequestModel);
    CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;
    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws Exception;

    ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateUtilization(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateAchievedValues(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> calculateChargeable(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> attachShipments(Long consolidationId, List<Long> shipmentIds) throws Exception;

    ResponseEntity<IRunnerResponse> detachShipments(Long consolidationId, List<Long> shipmentIds) throws Exception;

    ResponseEntity<IRunnerResponse> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception;

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException;

    void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate);

    void generateConsolidationNumber(ConsolidationDetails consolidationDetails);
    void autoGenerateEvents(ConsolidationDetails consolidationDetails);

    ResponseEntity<IRunnerResponse> getConsolFromShipment(Long shipmentId);

    ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws Exception;
    VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws Exception;
    ResponseEntity<IRunnerResponse> listPacksForAssignDetach(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> assignPacksAndShipments(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> detachPacksAndShipments(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getContainerPackSummary(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getDefaultConsolidation();
    ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber();
    ResponseEntity<IRunnerResponse> validateMawbNumber(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateConsoleBookingFields(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> showCreateBooking(String operation);
    ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel);
}