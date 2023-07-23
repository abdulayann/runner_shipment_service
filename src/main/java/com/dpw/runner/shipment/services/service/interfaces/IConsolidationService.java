package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface IConsolidationService extends ICommonService{
    List<ConsolidationDetails> createTestConsolidations(Integer count);
    ResponseEntity<?> fetchConsolidations(CommonRequestModel commonRequestModel);
    CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel);
    ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;
    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> calculateUtilization(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel);
    ResponseEntity<?> calculateChargeable(CommonRequestModel commonRequestModel);
}