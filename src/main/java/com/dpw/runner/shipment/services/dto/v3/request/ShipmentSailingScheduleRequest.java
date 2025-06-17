package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class ShipmentSailingScheduleRequest implements IRunnerRequest {
    private List<RoutingsRequest> routings = new ArrayList<>();
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime shippingInstructionCutoff;
    private LocalDateTime dgCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime earliestEmptyEquipmentPickUp;
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;
    private LocalDateTime latestArrivalTime;
    private String carrier;
    private String scacCode;
}
