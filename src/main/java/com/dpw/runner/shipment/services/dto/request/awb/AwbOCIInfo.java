package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.UUID;

@Data
@Builder
@ApiModel("AWB OCI Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbOCIInfo {
    private Integer id;
    private Integer entityId;
    private String entityType;
    private Integer informationIdentifier;
    private Integer tradeIdentificationCode;
    private String tradeIdentificationComment;
    private Integer version;
}
