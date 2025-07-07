package com.dpw.runner.shipment.services.migration.map.console.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.migration.map.console.ConsoleDBMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConsoleV2DBMapper extends ConsoleDBMapper<ConsolidationDetailsResponse> {
    @Override
    public ConsolidationDetails mapToEntity(ConsolidationDetailsResponse consolidationDetailsResponse) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(consolidationDetailsResponse.getId());
        log.info("Mapping Consolidation V2 DTO to Consolidation Entity for Id {}", consolidationDetailsResponse.getId());
        return consolidationDetails;
    }

    @Override
    public ConsolidationDetailsResponse mapFromEntity(ConsolidationDetails d) {
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(d.getId());
        log.info("Mapping Consolidation Entity  to Consolidation V2 DTO for Id {}", d.getId());
        return consolidationDetailsResponse;
    }
}
