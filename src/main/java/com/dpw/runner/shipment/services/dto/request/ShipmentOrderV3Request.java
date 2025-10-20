package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.v3.request.OrderLineV3Response;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@ApiModel("Order Number Request Model V3")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ShipmentOrderV3Request extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID orderGuid;
    private Long shipmentId;
    private String orderNumber;
    private LocalDateTime orderDate;
    private List<OrderLineV3Response> orderPackings;
}