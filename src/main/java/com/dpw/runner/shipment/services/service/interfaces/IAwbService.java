package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IAwbService {
    ResponseEntity<?> createAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> updateAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> list(CommonRequestModel commonRequestModel);
    ResponseEntity<?> createMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<?> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync);

    ResponseEntity<?> customAwbRetrieve(CommonRequestModel commonRequestModel);

    ResponseEntity<?> reset(CommonRequestModel commonRequestModel);
    ResponseEntity<?> partialAutoUpdateAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> partialAutoUpdateMawb(CommonRequestModel commonRequestModel);
}
