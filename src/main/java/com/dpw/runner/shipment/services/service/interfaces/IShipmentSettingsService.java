package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsService extends ICommonService{
    ResponseEntity<IRunnerResponse> uploadTemplate(CommonRequestModel commonRequestModel);
    ResponseEntity<?> downloadTemplate(String templateId);
    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> completeSettingsUpdateCreateV1(CommonRequestModel commonRequestModel) throws Exception;
    ResponseEntity<IRunnerResponse> retrieveByTenantId(CommonRequestModel commonRequestModel);
    }
