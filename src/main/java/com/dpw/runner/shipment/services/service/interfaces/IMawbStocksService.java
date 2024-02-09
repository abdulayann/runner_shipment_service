package com.dpw.runner.shipment.services.service.interfaces;

import org.springframework.http.ResponseEntity;

public interface IMawbStocksService extends ICommonService {
    ResponseEntity<?> getNextMawbNumberByCarrier(String airlinePrefix, String borrowedFrom);
}
