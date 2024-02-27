package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IAwbService {
    ResponseEntity<IRunnerResponse> createAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync);

    ResponseEntity<IRunnerResponse> customAwbRetrieve(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> reset(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> partialAutoUpdateAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> partialAutoUpdateMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment);
    ResponseEntity<IRunnerResponse> generateAwbPaymentInfo(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveByAwbByMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> generateUpdatedNatureAndQuantGoodsField(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> getChargeTypeMasterData(CommonGetRequest commonGetRequest);

}
