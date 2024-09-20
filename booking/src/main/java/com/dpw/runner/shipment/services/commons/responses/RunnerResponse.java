package com.dpw.runner.shipment.services.commons.responses;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RunnerResponse<T> implements IRunnerResponse {

    @ApiModelProperty(position = 1, name = "success")
    private boolean success;

    @ApiModelProperty(position = 2, name = "requestId")
    private String requestId;

    @ApiModelProperty(position = 3, name = "data")
    private T data;

    @ApiModelProperty(position = 4, name = "pageNo")
    private int pageNo;

    @ApiModelProperty(position = 5, name = "count")
    private long count;

    @ApiModelProperty(position = 6, name = "error")
    private ApiError error;

    @ApiModelProperty(position = 7, name = "warning")
    private String warning;

    @SuppressWarnings("unchecked")
    public T getData(@SuppressWarnings("rawtypes") Class clazz) {
        ObjectMapper objectMapper = new ObjectMapper();
        return (T) objectMapper.convertValue(data, clazz);
    }
}
