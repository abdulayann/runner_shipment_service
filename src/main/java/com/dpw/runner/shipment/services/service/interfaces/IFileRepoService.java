package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface IFileRepoService extends ICommonService {
    ResponseEntity<IRunnerResponse> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> uploadDocument(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> downloadDocument(CommonRequestModel commonRequestModel);
}
