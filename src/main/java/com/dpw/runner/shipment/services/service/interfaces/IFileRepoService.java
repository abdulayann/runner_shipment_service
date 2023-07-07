package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entity.FileRepo;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface IFileRepoService extends ICommonService, IShipStitchService{
    ResponseEntity<?> retrieveByEntityIdAndEntityType(CommonRequestModel commonRequestModel);
}
