package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface ITasksService extends ICommonService{
    ResponseEntity<?> createTask(CommonRequestModel commonRequestModel);
}