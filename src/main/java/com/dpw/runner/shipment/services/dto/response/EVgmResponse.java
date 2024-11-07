package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("EVgm Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EVgmResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long bookingId;
    private Long containerId;
    private Long shippingInstructionId;
    private String status;
    private PartiesResponse responsibleParty;
    private PartiesResponse authorizedParty;
    private String approvalSignature;
    private LocalDateTime vgmDeterminationDateTime;
    private Boolean delegate;
    private String carrier;
    private Integer weightMethod;
    private List<EventsResponse> eventsList;
}
