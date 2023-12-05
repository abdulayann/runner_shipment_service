package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@Builder
@ApiModel("AWB Special Handling Codes Mapping Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbSpecialHandlingCodesMappingInfo {
    private Long entityId;
    private String entityType;
    private String shcId;
}
