package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.Map;

@Data
@Builder
@ApiModel("Report Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ReportResponse implements IRunnerResponse {
    private byte[] content;
    Map<String, Object> documentServiceMap;
}