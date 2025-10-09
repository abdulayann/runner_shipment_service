package com.dpw.runner.shipment.services.dto.v3.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ApiModel(value = "OrderLine create update delete request model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderLineCreateUpdateDeleteRequest {
    private String orderId;
    private String orderNo;

    private List<OrderLineV3Response> createOrderLines;
    private List<OrderLineV3Response> updateOrderLines;
    private List<OrderLineV3Response> deleteOrderLines;
}
