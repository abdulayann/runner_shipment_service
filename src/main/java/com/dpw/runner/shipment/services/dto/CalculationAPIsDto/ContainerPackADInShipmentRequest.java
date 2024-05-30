package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class ContainerPackADInShipmentRequest implements IRunnerRequest {
    Long shipmentId;
    Long containerId;
    private List<Long> packsId;
}
