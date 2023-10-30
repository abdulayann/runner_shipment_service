package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Carrier Details Response Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CarrierDetailResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String shippingLine;
    private String vessel;
    private String voyage;
    private String flightNumber;
    private String aircraftType;
    private String aircraftRegistration;
    private String truckRefNumber;
    private String journeyNumber;
    private String journeyRefNumber;
    private String origin;
    private String destination;
    @ExcludeTimeZone
    private LocalDateTime eta;
    @ExcludeTimeZone
    private LocalDateTime etd;
    @ExcludeTimeZone
    private LocalDateTime ata;
    @ExcludeTimeZone
    private LocalDateTime atd;
    private String originPort;
    private String destinationPort;
    private String originPortName;
    private String destinationPortName;
    public Map<String, String> masterData;
    public Map<String, String> unlocationData;
    public Map<String, String> carrierMasterData;
    public Map<String, String> vesselsMasterData;
}
