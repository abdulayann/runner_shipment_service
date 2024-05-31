package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * /** * Generic response.
 *
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor @Generated
@ApiModel(description= "Runner Response Model")
public class RunnerResponse {
    @ApiModelProperty(position = 1, name = "success")
    private boolean success;

    @ApiModelProperty(position = 6, name = "error")
    private ApiError error;

}

