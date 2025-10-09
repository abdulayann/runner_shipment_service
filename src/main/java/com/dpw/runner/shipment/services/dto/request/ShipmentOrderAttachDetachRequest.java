package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.v3.request.OrderLineV3Response;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Schema("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SuppressWarnings("java:S1948")
public class ShipmentOrderAttachDetachRequest implements IRunnerRequest {
    private UUID shipmentGuid;
    private String event;
    private List<OrderDetails> orderDetailsList;
    private List<OrderDetails> orderDetailsForAttach;
    private List<OrderDetails> orderDetailsForDetach;

    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class OrderDetails {
        private String orderNumber;
        private UUID orderGuid;
        private Long shipmentId;
        private LocalDateTime orderDate;
        private List<OrderLineV3Response> orderPackings;
    }
}
