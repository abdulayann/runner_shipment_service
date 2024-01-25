package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("AWB OCI Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOCIInfo {
    private Long entityId;
    private String entityType;
    private Integer informationIdentifier;
    private Integer tradeIdentificationCode;
    private String tradeIdentificationComment;
}
