package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public interface IShipmentService extends ICommonService {
    List<ShipmentDetails> createTestShipment(Integer count);

    ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel);

    void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException;

    CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException;

    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel);

    ResponseEntity<?> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> cloneShipment(CommonRequestModel commonRequestModel);
    ResponseEntity<?> transportInstructionList(CommonRequestModel commonRequestModel);
    ResponseEntity<?> containerListForTI(CommonRequestModel commonRequestModel);
    void afterSave(ShipmentDetails shipmentDetails, boolean isCreate);
    ResponseEntity<?> fullShipmentsList(CommonRequestModel commonRequestModel);
    ResponseEntity<?> assignShipmentContainers(CommonRequestModel commonRequestModel);
    ResponseEntity<?> retrieveByOrderId(String orderId);
}