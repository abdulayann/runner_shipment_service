package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import org.springframework.http.ResponseEntity;

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
    ResponseEntity<?> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment);
    ResponseEntity<?> generateAwbPaymentInfo(CommonRequestModel commonRequestModel);
    ResponseEntity<?> retrieveByAwbByMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<?> generateUpdatedNatureAndQuantGoodsField(CommonRequestModel commonRequestModel);

}
