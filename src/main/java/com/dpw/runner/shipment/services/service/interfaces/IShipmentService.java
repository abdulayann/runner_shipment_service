package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface IShipmentService extends ICommonService{
    List<ShipmentDetails> createTestShipment(Integer count);
    ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;

    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel);
}