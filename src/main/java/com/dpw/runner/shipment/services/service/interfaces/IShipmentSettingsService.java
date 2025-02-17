package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IShipmentSettingsService extends ICommonService {
    ResponseEntity<IRunnerResponse> uploadTemplate(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> downloadTemplate(String templateId);

    ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> completeSettingsUpdateCreateV1(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> retrieveByTenantId(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> listCoLoadStationTenantIds();

    ResponseEntity<IRunnerResponse> listHubTenantIds();

    ResponseEntity<IRunnerResponse> hideManifest(boolean hideManifest);

    ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel);
}
