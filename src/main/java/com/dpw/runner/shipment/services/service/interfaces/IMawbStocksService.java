package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IMawbStocksService extends ICommonService {
    ResponseEntity<IRunnerResponse> createV1MawbStocks(CommonRequestModel commonRequestModel, Boolean checkForSync);
    ResponseEntity<IRunnerResponse> getNextMawbNumberByCarrier(String airlinePrefix, String borrowedFrom);
}
