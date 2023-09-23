package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IFusionServiceAdapter {
    ResponseEntity<?> checkCreditLimitP100(CommonRequestModel requestModel) throws Exception;
}
