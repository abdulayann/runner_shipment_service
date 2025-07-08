package com.dpw.runner.shipment.services.migration.map.console.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConsolidationDTOV2ToV3Mapper implements IDTOMigrationMapper<ConsolidationDetailsResponse, ConsolidationDetailsV3Response> {
    @Override
    public ConsolidationDetailsV3Response forwardMap(ConsolidationDetailsResponse consolidationDetailsResponse) {
        ConsolidationDetailsV3Response consolidationDetailsV3Response = new ConsolidationDetailsV3Response();
        consolidationDetailsV3Response.setId(consolidationDetailsResponse.getId());
        log.info("Mapping Consolidation V2 DTO to Consolidation V3 for ID {}", consolidationDetailsV3Response.getId());
        return consolidationDetailsV3Response;
    }

    @Override
    public ConsolidationDetailsResponse reverseMap(ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        ConsolidationDetailsResponse consolidationDetailsResponse = new ConsolidationDetailsResponse();
        consolidationDetailsResponse.setId(consolidationDetailsV3Response.getId());
        log.info("Mapping Consolidation V3 DTO to Consolidation V2 for ID {}", consolidationDetailsV3Response.getId());
        return consolidationDetailsResponse;
    }
}
