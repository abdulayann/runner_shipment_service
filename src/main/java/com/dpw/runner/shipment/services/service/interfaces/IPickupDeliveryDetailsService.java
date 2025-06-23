package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;


public interface IPickupDeliveryDetailsService extends ICommonService {
    ResponseEntity<IRunnerResponse> createV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> updateV2(CommonRequestModel commonRequestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> deleteV2(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> retrieveByIdV2(CommonRequestModel commonRequestModel);
}
