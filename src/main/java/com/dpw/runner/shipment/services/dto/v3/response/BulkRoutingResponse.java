package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingLegWarning;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;
import java.util.Map;

@Data
@Builder
@Schema(description = "Bulk Routing Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkRoutingResponse implements IRunnerResponse {
    List<RoutingsResponse> routingsResponseList;
    String message;
    String warningMessage;
    Map<String, RoutingLegWarning> legsWarning;
}
