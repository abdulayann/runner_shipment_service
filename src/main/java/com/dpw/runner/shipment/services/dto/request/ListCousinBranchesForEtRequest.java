package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ListCousinBranchesForEtRequest implements IRunnerRequest {
    private String containsText;
    private List<String> includeColumns;
    private Long entityId;
    private String entityType;
    private List<String> sort;
    private int take = 500;
    private int skip;
    private int columnSelection;
    private Boolean excludeTotalCount;
    private Boolean isReassign;
    private Boolean isReceivingBranch;
    private Boolean isTriangulationBranch;
    private List<Object> criteria;
}
