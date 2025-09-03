package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

public interface ICustomerBookingService {
    ResponseEntity<IRunnerResponse> platformCreateBooking(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> retryForBilling(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> cloneBooking(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException;

    Optional<CustomerBooking> findById(Long bookingId);
}
