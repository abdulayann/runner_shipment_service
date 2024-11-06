package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@ApiModel("EVgm Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class EVgmRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long bookingId;
    private Long containerId;
    private Long shippingInstructionId;
    private String status;
    private PartiesRequest responsibleParty;
    private PartiesRequest authorizedParty;
    private String approvalSignature;
    private LocalDateTime vgmDeterminationDateTime;
    private Boolean delegate;
    private String carrier;
    private Integer weightMethod;
    private List<EventsRequest> eventsList;
}
