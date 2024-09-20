package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;


@Data
@Builder
@ApiModel("Arrival Departure Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ArrivalDepartureDetailsRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private PartiesRequest containerYardId;
    private PartiesRequest transportPortId;
    private PartiesRequest CTOId;
    private PartiesRequest CFSId;
    private PartiesRequest firstForeignPortId;
    private PartiesRequest lastForeignPortId;
    private String firstForeignPort;
    private String lastForeignPort;
    private String type;
    private LocalDateTime firstForeignPortArrivalDate;
    private LocalDateTime lastForeignPortDepartureDate;
}