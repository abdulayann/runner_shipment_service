package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@ApiModel("Arrival Departure Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ArrivalDepartureDetailsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private PartiesResponse dContainerYardId;
    private PartiesResponse dTransportPortId;
    private PartiesResponse dFirstForeignPortId;
    private PartiesResponse dLastForeignPortId;
    private PartiesResponse aContainerYardId;
    private PartiesResponse aTransportPortId;
    private PartiesResponse aFirstArrivalPortId;
    private PartiesResponse aLastForeignPortId;
    private LocalDateTime dFirstForeignPortArrivalDate;
    private LocalDateTime dLastForeignPortDepartureDate;
    private LocalDateTime aFirstArrivalPortArrivalDate;
    private LocalDateTime aLastForeignPortDepartureDate;
}
