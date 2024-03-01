package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

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
    private String mode;
    private String routingStatus;
    private String vesselName;
    private String pol;
    private String pod;
    private boolean isDomestic;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime etd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime ata;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> carrierMasterData;
}
