package com.dpw.runner.shipment.services.commons.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("AWB Special handling Codes Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbSpecialHandlingCodesInfo {
    private Integer entityId;
    private String entityType;
    private Long shcId;
}
