package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface ISyncQueueService {
    ResponseEntity<?> saveSyncRequest(String moduleType, String moduleId, Object data);
    ResponseEntity<?> triggerSyncRequest(CommonRequestModel commonRequestModel);
}
