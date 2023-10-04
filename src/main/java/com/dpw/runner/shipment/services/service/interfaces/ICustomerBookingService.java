package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;

public interface ICustomerBookingService {
    ResponseEntity<?> platformCreateBooking(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<?> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<?> checkCreditLimitFromFusion(CommonRequestModel commonRequestModel) throws Exception;
}
