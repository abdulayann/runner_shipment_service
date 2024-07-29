package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@ApiModel("Order Number Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class OrderNumberResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long orderNumber;
    private LocalDateTime createdAt;
}
