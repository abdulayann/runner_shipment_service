package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface IFileRepoService extends ICommonService {
    ResponseEntity<IRunnerResponse> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> uploadDocument(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> downloadDocument(CommonRequestModel commonRequestModel);
}
