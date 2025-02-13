package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@ApiModel("Routings Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class RoutingsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private Long bookingId;
    private Long leg;
    private RoutingCarriage carriage;
    private String mode;
    private String routingStatus;
    private String vesselName;
    private String pol;
    private String pod;
    @JsonProperty("domestic")
    private boolean isDomestic;
    @ExcludeTimeZone
    private LocalDateTime eta;
    @ExcludeTimeZone
    private LocalDateTime etd;
    @ExcludeTimeZone
    private LocalDateTime ata;
    @ExcludeTimeZone
    private LocalDateTime atd;
    private Long consolidationId;
    private Boolean isLinked;
    @Builder.Default
    private Boolean isSelectedForDocument = false;
    private String voyage;
    private String aircraftRegistration;
    private String flightNumber;
    private String aircraftType;
    private String vehicleNumber;
    private String entityType;
    private Long entityId;
    private Long routeLegId;
    private Long transitDays;
    private String carrier;
    private String truckReferenceNumber;
    private String carrierCountry;
    private String originPortLocCode;
    private String destinationPortLocCode;
    private Boolean inheritedFromConsolidation;

    public void setIsDomestic(boolean isDomestic) {
        this.isDomestic = isDomestic;
    }
}
