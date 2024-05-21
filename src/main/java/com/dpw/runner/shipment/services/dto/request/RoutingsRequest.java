package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
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
    private String mode;
    private String routingStatus;
    private String vesselName;
    private String pol;
    private String pod;
    @JsonProperty("domestic")
    private boolean isDomestic;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;
    private Long consolidationId;
    private Boolean isLinked;
    private String voyage;
    private String aircraftRegistration;
    private String flightNumber;
    private String aircraftType;
    private String entityType;
    private Long entityId;
    private Long routeLegId;
    private Long transitDays;
    private String carrier;
    private String truckReferenceNumber;
    private String carrierCountry;
    
    public void setIsDomestic(boolean isDomestic) {
        this.isDomestic = isDomestic;
    }
}
