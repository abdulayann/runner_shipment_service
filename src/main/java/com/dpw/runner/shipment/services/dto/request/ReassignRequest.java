package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.Map;

@Getter
@Setter
@Schema(description = "Request for Reassign Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ReassignRequest implements IRunnerRequest {
    private Long id;
    private Integer branchId;
    private String remarks;
    private Map<String, Integer> shipmentGuidReassignBranch;
}
