package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.UnlocationData;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;

@Data
@Builder
@ApiModel("AWB Other Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbOtherInfo {
    private Long entityId;
    private String entityType;
    private String shipper;
    private String carrier;
    @UnlocationData
    private String executedAt;
    private LocalDateTime executedOn;
}
