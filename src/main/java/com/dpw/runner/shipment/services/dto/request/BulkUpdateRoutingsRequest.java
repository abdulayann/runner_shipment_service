package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.List;

@Data
@Builder
@ApiModel("Routings Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class BulkUpdateRoutingsRequest extends CommonRequest implements IRunnerRequest {
    private List<RoutingsRequest> routings;
    private Long entityId;
}
