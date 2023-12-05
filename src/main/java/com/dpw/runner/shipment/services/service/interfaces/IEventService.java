package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IEventService extends ICommonService {
    ResponseEntity<?> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception;

}
