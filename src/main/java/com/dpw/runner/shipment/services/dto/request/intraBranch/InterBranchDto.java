package com.dpw.runner.shipment.services.dto.request.intraBranch;

import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class InterBranchDto {

    /**
     * When standing on regional/coload branch, I want to see all the consolidations available for current branch, along with
     * consolidation on all the hub branches where current branch is registered as coload
     */
    private boolean isCoLoadStation;
    private List<Integer> hubTenantIds;

    /**
     * When standing on hub/central branch, I want to access all the shipments available for current branch, along with
     * shipments on all the coloading stations.
     */
    private boolean isHub;
    private List<Integer> coloadStationsTenantIds;

}
