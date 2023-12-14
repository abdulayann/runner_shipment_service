package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IRoutingsService extends ICommonService {
    ResponseEntity<?> V1RoutingsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception;

}
