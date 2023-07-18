package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IBookingCarriageService extends ICommonService {
    ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel);
}
