package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
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
    ResponseEntity<?> fetchConsolidations(CommonRequestModel commonRequestModel);
    CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel);
    ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;
    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateUtilization(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateAchievedValues(CommonRequestModel commonRequestModel);

    ResponseEntity<?> calculateChargeable(CommonRequestModel commonRequestModel);

    ResponseEntity<?> attachShipments(Long consolidationId, List<Long> shipmentIds) throws Exception;

    ResponseEntity<?> detachShipments(Long consolidationId, List<Long> shipmentIds) throws Exception;

    ResponseEntity<?> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception;

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException;

    void afterSave(ConsolidationDetails consolidationDetails, boolean isCreate);

    void generateConsolidationNumber(ConsolidationDetails consolidationDetails);

    ResponseEntity<?> getConsolFromShipment(Long shipmentId);

    ResponseEntity<?> calculateContainerSummary(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> calculatePackSummary(CommonRequestModel commonRequestModel) throws Exception;
    VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws Exception;
    ResponseEntity<?> listPacksForAssignDetach(CommonRequestModel commonRequestModel);
    ResponseEntity<?> assignPacksAndShipments(CommonRequestModel commonRequestModel);
    ResponseEntity<?> detachPacksAndShipments(CommonRequestModel commonRequestModel);
}