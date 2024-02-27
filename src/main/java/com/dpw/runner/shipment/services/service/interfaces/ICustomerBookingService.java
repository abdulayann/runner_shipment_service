package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;

public interface ICustomerBookingService {
    ResponseEntity<IRunnerResponse> platformCreateBooking(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(CommonRequestModel commonRequestModel) throws Exception;
}
