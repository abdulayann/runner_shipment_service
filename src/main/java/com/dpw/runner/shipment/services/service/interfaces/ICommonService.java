package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;

@SuppressWarnings("ALL")
public interface ICommonService {
    ResponseEntity<?> create(CommonRequestModel commonRequestModel);

    ResponseEntity<?> update(CommonRequestModel commonRequestModel);

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);

    CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel);

    ResponseEntity<?> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);
}
