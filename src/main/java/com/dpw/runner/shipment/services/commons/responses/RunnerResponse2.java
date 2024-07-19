package com.dpw.runner.shipment.services.commons.responses;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@ApiModel(description = "Details about the response1")
public class RunnerResponse2 {

    @ApiModelProperty(position = 1, name = "success")
    private boolean success;

    @ApiModelProperty(position = 2, name = "requestId")
    private String requestId;

    @ApiModelProperty(position = 3, name = "data")
    private Object data;

    @ApiModelProperty(position = 4, name = "pageNo")
    private int pageNo;

    @ApiModelProperty(position = 5, name = "count")
    private long count;

    @ApiModelProperty(position = 6, name = "error")
    private ApiError error;
}
