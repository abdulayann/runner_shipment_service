package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@Builder
@ApiModel("AWB OCI Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOCIInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private Integer informationIdentifier;
    private Integer tradeIdentificationCode;
    private String tradeIdentificationComment;
}
