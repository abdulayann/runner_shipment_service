package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
    private String carrier;
    public Map<String, String> unlocationData;
    public Map<String, String> masterData;
    public Map<String, String> carrierMasterData;
}
