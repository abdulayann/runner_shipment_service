package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Builder
@Schema("AWB Special handling Codes Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbSpecialHandlingCodesInfo {
    private Integer entityId;
    private String entityType;
    private Long shcId;
}
