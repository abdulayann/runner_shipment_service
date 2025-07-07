package com.dpw.runner.shipment.services.migration.map.console.v3ToV2;

import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.migration.map.console.ConsoleDBMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConsoleV3DBMapper extends ConsoleDBMapper<ConsolidationDetailsV3Response> {

    @Override
    public ConsolidationDetails mapToEntity(ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(consolidationDetailsV3Response.getId());
        log.info("Mapping Consolidation V3 DTO to Consolidation Entity  for Id {}", consolidationDetailsV3Response.getId());
        return consolidationDetails;
    }

    @Override
    public ConsolidationDetailsV3Response mapFromEntity(ConsolidationDetails d) {
        ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
        consolidationDetailsV3Response.setId(d.getId());
        log.info("Mapping Consolidation Entity  to Consolidation V3 DTO for Id {}", d.getId());
        return consolidationDetailsV3Response;
    }
}