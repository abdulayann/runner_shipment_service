package com.dpw.runner.shipment.services.dto.response;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@EqualsAndHashCode(callSuper = true)
@Data
@Setter
@Getter
@ApiModel("Container Response V3 Model")
public class ContainerBaseV3Response extends ContainerBaseResponse {
    private Long attachedShipmentId;
    private String attachedShipmentNumber;
    private String attachedShipmentType;
}
