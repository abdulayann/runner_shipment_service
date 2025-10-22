package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import io.swagger.annotations.ApiModel;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ApiModel("Order Number Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ShipmentOrderRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID orderGuid;
    private Long shipmentId;
    private String orderNumber;
    private LocalDateTime orderDate;
    private List<PackingResponse> orderPackings;
}
