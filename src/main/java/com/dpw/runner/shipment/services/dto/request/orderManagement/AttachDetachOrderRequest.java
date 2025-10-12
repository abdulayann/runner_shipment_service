package com.dpw.runner.shipment.services.dto.request.orderManagement;

import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AttachDetachOrderRequest {

    private UUID shipmentGuid;
    private Long shipmentId;

    private List<OrderInfo> attachOrderInfo;
    private List<OrderInfo> detachOrderInfo;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class OrderInfo {

        private UUID orderGuid;
        private String orderId;
        private String orderNumber;
    }
}
