package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IELDetailsService extends ICommonService, IShipStitchService {
    ResponseEntity<?> validateElNumber(CommonRequestModel commonRequestModel);
}

