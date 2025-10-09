package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.Map;

@Data
@Builder
@Schema("Report Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ReportResponse implements IRunnerResponse {
    private byte[] content;
    Map<String, Object> documentServiceMap;
}