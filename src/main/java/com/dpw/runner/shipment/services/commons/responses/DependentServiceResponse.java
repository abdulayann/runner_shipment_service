package com.dpw.runner.shipment.services.commons.responses;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DependentServiceResponse implements IRunnerResponse {
    @ApiModelProperty(position = 1, name = "success")
    private boolean success;

    @ApiModelProperty(position = 2, name = "requestId")
    private String requestId;

    @ApiModelProperty(position = 3, name = "data")
    private Object data;

    @ApiModelProperty(position = 4, name = "count")
    private long numberOfRecords;

    @ApiModelProperty(position = 5, name = "pageSize")
    private long pageSize;

    @ApiModelProperty(position = 6, name = "pageNo")
    private long pageNo;

    @ApiModelProperty(position = 7, name = "error")
    private ApiError error;
}
