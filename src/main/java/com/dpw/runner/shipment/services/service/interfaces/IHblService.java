package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Set;

public interface IHblService extends ICommonService {

    ResponseEntity<IRunnerResponse> generateHBL(CommonRequestModel commonRequestModel) throws RunnerException;
    ResponseEntity<IRunnerResponse> retrieveByShipmentId(CommonRequestModel buildRequest);
    ResponseEntity<IRunnerResponse> resetHbl(CommonRequestModel buildRequest) throws RunnerException;
    ResponseEntity<IRunnerResponse> saveV1Hbl(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException;
    Hbl checkAllContainerAssigned(ShipmentDetails shipment, Set<Containers> containersList, List<Packing> packings);
    ResponseEntity<IRunnerResponse> partialUpdateHBL(CommonRequestModel commonRequestModel) throws RunnerException;

}
