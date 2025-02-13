package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IMawbStocksService extends ICommonService {
    ResponseEntity<IRunnerResponse> createV1MawbStocks(CommonRequestModel commonRequestModel, Boolean checkForSync) throws RunnerException;

    ResponseEntity<IRunnerResponse> getNextMawbNumberByCarrier(String airlinePrefix, String borrowedFrom);
}
