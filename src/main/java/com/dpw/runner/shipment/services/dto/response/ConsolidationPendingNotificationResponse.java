package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationPendingNotificationResponse extends ConsolidationDetailsResponse{
    private Long id;
    private UUID guid;
    private Integer tenantId;
    private Boolean hazardous;
    private String consolidationNumber;
    private Boolean interBranchConsole;
    private Long receivingBranch;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private Map<String, Object> tenantMasterData;
}
