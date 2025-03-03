package com.dpw.runner.shipment.services.commons.responses;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class MDMServiceResponse extends DependentServiceResponse{

    @ApiModelProperty(name = "message")
    private String message;

}
