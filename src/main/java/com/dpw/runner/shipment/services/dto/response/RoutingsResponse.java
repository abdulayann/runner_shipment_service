package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@ApiModel("Routings Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class RoutingsResponse implements IRunnerResponse {
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime etd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime ata;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime atd;
    private Long consolidationId;
    private Boolean isLinked;
    private Boolean isSelectedForDocument;
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
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> carrierMasterData;
    private String originPortLocCode;
    private String destinationPortLocCode;
    private Boolean inheritedFromConsolidation;

    public void setIsDomestic(boolean isDomestic) {
        this.isDomestic = isDomestic;
    }
}
