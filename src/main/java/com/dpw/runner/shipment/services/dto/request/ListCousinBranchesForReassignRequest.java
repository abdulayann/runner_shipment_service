package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ListCousinBranchesForReassignRequest implements IRunnerRequest {
    private String containsText;
    private List<String> includeColumns;
    private Long entityId;
    private String entityType;
    private List<String> sort;
    private int take = 500;
    private int skip;
    private int columnSelection;
    private Boolean excludeTotalCount;
}
