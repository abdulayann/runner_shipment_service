package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@Schema("Order Number Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class OrderNumberResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long orderNumber;
    private LocalDateTime createdAt;
}
