package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.persistence.Column;
import java.time.LocalDateTime;

@Data
@Builder
@ApiModel("Routings Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class RoutingsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private Long leg;
    private String mode;
    private String routingStatus;
    private String vesselName;
    private Long polId;
    private Long podId;
    private boolean isDomestic;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;
    private Long consolidation_id;
    private Boolean isLinked;
    private String voyage;
    private String aircraftRegistration;
    private String flightNumber;
    private String aircraftType;
    private String entityType;
    private Long entityId;
    private Long routeLegId;
    private Long vesselId;
    private Long transitDays;
}
