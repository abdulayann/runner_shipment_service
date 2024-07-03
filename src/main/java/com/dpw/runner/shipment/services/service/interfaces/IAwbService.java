package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.Optional;

public interface IAwbService {
    ResponseEntity<IRunnerResponse> createAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateAwb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> updateGoodsAndPacksForMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> createV1Awb(CommonRequestModel commonRequestModel, boolean checkForSync);

    ResponseEntity<IRunnerResponse> customAwbRetrieve(CommonRequestModel commonRequestModel);

    ResponseEntity<IRunnerResponse> reset(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> partialAutoUpdateAwb(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> partialAutoUpdateMawb(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel, boolean isShipment);
    ResponseEntity<IRunnerResponse> generateAwbPaymentInfo(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> retrieveByAwbByMawb(CommonRequestModel commonRequestModel);
    ResponseEntity<IRunnerResponse> generateUpdatedNatureAndQuantGoodsField(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> getChargeTypeMasterData(CommonGetRequest commonGetRequest) throws RunnerException;
    ResponseEntity<IRunnerResponse> validateIataAgent(Boolean fromShipment, Optional<Long> consolidaitonId);
    Awb getMawnLinkPacks(Awb awb);
    ResponseEntity<IRunnerResponse> getFnmStatusMessage(Optional<Long> shipmentId, Optional<Long> consolidaitonId);
    ResponseEntity<IRunnerResponse> getFetchIataRates(CommonRequestModel commonRequestModel) throws RunnerException;
}
