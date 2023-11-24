package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface IHblService extends ICommonService {

    ResponseEntity<?> generateHBL(CommonRequestModel commonRequestModel);
    ResponseEntity<?> retrieveByShipmentId(CommonRequestModel buildRequest);
    ResponseEntity<?> resetHbl(CommonRequestModel buildRequest);
    ResponseEntity<?> saveV1Hbl(CommonRequestModel commonRequestModel) throws Exception;
    void checkAllContainerAssigned(Long shipmentId, List<Containers> containersList, List<Packing> packings);
    ResponseEntity<?> partialUpdateHBL(CommonRequestModel commonRequestModel);

}
