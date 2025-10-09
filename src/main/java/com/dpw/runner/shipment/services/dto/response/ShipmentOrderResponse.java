package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
@Builder
@Schema(description = "Order Number Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentOrderResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private UUID orderGuid;
    private Long shipmentId;
    private String orderNumber;
    private LocalDateTime orderDate;
    private List<PackingResponse> orderPackings;
}
