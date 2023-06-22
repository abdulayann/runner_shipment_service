package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.springframework.http.ResponseEntity;

import java.util.Optional;

public interface IViewsService {
    ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception;

    ResponseEntity<?> list(CommonRequestModel commonRequestModel);

    ResponseEntity<?> delete(CommonRequestModel commonRequestModel);

    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);
}
