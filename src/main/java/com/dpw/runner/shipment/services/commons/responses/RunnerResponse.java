package com.dpw.runner.shipment.services.commons.responses;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings( "java:S1948")
@JsonPropertyOrder({ "success", "requestId", "data", "pageNo", "count", "error", "warning" })
public class RunnerResponse<T> implements IRunnerResponse {

    @SchemaProperty(  name = "success")
    private boolean success;

    @SchemaProperty( name = "requestId")
    private String requestId;

    @SchemaProperty(  name = "data")
    private T data;

    @SchemaProperty( name = "pageNo")
    private int pageNo;

    @SchemaProperty(  name = "count")
    private long count;

    @SchemaProperty( name = "error")
    private ApiError error;

    @SchemaProperty(  name = "warning")
    private String warning;

    @SuppressWarnings("unchecked")
    public T getData(@SuppressWarnings("rawtypes") Class clazz) {
        ObjectMapper objectMapper = new ObjectMapper();
        return (T) objectMapper.convertValue(data, clazz);
    }
}
