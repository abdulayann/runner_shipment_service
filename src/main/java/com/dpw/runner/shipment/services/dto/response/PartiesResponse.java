package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Parties Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class PartiesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long entityId;
    private String entityType;
    private String type;
    private Integer orgId;
    private Integer tenantId;
    private Integer addressId;
    private List<Containers> containersPickupAddress;
    private List<Containers> containersDeliveryAddress;
}
