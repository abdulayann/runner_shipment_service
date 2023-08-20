package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
public class CustomerBookingService implements ICustomerBookingService {

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> platformCreateBooking(CommonRequestModel commonRequestModel) {
        PlatformToRunnerCustomerBookingRequest request = (PlatformToRunnerCustomerBookingRequest) commonRequestModel.getData();
        return null;
    }
}
