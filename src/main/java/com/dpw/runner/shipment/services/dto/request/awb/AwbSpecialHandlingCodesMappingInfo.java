package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.io.Serializable;

@Data
@Builder
@Schema(description = "AWB Special Handling Codes Mapping Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbSpecialHandlingCodesMappingInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String shcId;
}
