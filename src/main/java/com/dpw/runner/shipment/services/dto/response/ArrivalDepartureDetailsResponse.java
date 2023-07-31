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
    private PartiesResponse containerYardId;
    private PartiesResponse transportPortId;
    private PartiesResponse CFSId;
    private PartiesResponse CTOId;
    private PartiesResponse firstForeignPortId;
    private PartiesResponse lastForeignPortId;
    private String firstForeignPort;
    private String lastForeignPort;
    private String type;
    private LocalDateTime firstForeignPortArrivalDate;
    private LocalDateTime lastForeignPortDepartureDate;
}
