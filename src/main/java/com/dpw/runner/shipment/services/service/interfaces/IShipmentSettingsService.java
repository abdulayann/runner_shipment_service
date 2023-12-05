package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsService extends ICommonService{
    ResponseEntity<?> uploadTemplate(CommonRequestModel commonRequestModel);
    ResponseEntity<?> downloadTemplate(String templateId);
    ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> completeSettingsUpdateCreateV1(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<?> retrieveByTenantId(CommonRequestModel commonRequestModel);
    }
