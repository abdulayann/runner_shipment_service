package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
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
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;
}
