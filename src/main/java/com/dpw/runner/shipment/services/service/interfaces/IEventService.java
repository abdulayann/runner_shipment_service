package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Optional;

public interface IEventService extends ICommonService {
    ResponseEntity<?> V1EventsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception;
    ResponseEntity<?> trackEvents(Optional<Long> shipmentId, Optional<Long> consolidationId);
    void updateAtaAtdInShipment(List<Events> events, ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSettings);
}
