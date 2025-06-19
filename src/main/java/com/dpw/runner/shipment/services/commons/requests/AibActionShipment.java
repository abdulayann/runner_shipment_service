package com.dpw.runner.shipment.services.commons.requests;

import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AibActionShipment implements IRunnerRequest{

    private List<Long> consoleIdsList;
    private Long shipmentId;
    private ShipmentRequestedType shipmentRequestedType;
    private String rejectRemarks;
}
