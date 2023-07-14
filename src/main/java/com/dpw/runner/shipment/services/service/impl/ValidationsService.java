package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.service.interfaces.IValidationsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;


@SuppressWarnings("ALL")
@Service
@Slf4j
public class ValidationsService implements IValidationsService {

    // TODO: To implement this + controller
    @Autowired
    private IValidationsDao validationsDao;

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
}
