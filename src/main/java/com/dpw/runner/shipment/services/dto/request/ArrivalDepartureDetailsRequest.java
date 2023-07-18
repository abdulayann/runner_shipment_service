package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.apache.poi.hpsf.Decimal;

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
    private PartiesRequest dContainerYardId;
    private PartiesRequest dTransportPortId;
    private PartiesRequest dFirstForeignPortId;
    private PartiesRequest dLastForeignPortId;
    private PartiesRequest aContainerYardId;
    private PartiesRequest aTransportPortId;
    private PartiesRequest aFirstArrivalPortId;
    private PartiesRequest aLastForeignPortId;
    private LocalDateTime dFirstForeignPortArrivalDate;
    private LocalDateTime dLastForeignPortDepartureDate;
    private LocalDateTime aFirstArrivalPortArrivalDate;
    private LocalDateTime aLastForeignPortDepartureDate;
}