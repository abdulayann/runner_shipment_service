package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Containers;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;

@Data
@ApiModel("Parties Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class PartiesRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long entityId;
    private String entityType;
    private String type;
    private Integer orgId;
    private Integer tenantId;
    private Integer addressId;
    private List<Containers> containersPickupAddress;
    private List<Containers> containersDeliveryAddress;
}
