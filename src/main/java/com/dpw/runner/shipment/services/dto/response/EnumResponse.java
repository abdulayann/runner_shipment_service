package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.List;
import java.util.Map;


@Data
@Builder
@Schema(description = "Enum Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EnumResponse implements IRunnerResponse {
    private Map<String, List<EnumConstantResponse>> dataMap;
}
