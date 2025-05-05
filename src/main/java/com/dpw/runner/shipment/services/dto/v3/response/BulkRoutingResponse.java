package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;

@Data
@Builder
@ApiModel("Bulk Routing Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkRoutingResponse implements IRunnerResponse {
    List<RoutingsResponse> routingsResponseList;
    String message;
}
