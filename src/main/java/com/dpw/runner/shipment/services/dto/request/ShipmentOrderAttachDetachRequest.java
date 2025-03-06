package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
@ApiModel("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SuppressWarnings("java:S1948")
public class ShipmentOrderAttachDetachRequest implements IRunnerRequest {
    private UUID shipmentGuid;
    private String event;
    private List<OrderDetails> orderDetailsList;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class OrderDetails {
        private String orderNumber;
        private UUID orderGuid;
    }
}
