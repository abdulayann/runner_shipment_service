package com.dpw.runner.shipment.services.commons.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@Builder
@ApiModel("AWB Special Handling Codes Mapping Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbSpecialHandlingCodesMappingInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private String shcId;
}
