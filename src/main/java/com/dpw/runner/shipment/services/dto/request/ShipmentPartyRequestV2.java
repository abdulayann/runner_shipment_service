package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ShipmentPartyRequestV2 implements IRunnerRequest {

    @NotNull(message = "shipment_id is mandatory")
    private Long shipmentId;

    @NotNull(message = "tenant_id is mandatory")
    private Integer tenantId;

    @NotEmpty(message = "parties list cannot be empty")
    @Valid
    private List<PartiesData> parties;
}
