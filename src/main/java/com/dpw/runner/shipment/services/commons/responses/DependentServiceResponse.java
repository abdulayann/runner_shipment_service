package com.dpw.runner.shipment.services.commons.responses;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
@JsonPropertyOrder({ "success", "requestId", "data", "count", "pageSize", "pageNo", "error" })
public class DependentServiceResponse implements IRunnerResponse{
    @Schema(  name = "success")
    private boolean success;

    @Schema( name = "requestId")
    private String requestId;

    @Schema(name = "data")
    private Object data;

    @Schema(name = "count")
    private long numberOfRecords;

    @Schema(name = "pageSize")
    private long pageSize;

    @Schema(name = "pageNo")
    private long pageNo;

    @Schema(name = "error")
    private ApiError error;
}
