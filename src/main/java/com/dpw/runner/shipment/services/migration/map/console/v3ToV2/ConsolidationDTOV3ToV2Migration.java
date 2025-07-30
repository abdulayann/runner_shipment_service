package com.dpw.runner.shipment.services.migration.map.console.v3ToV2;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.console.v2ToV3.ConsolidationDTOV2ToV3Mapper;

public class ConsolidationDTOV3ToV2Migration implements IDTOMigrationMapper<ConsolidationDetailsV3Response, ConsolidationDetailsResponse> {

    private ConsolidationDTOV2ToV3Mapper dtov2ToV3Migration;

    public ConsolidationDTOV3ToV2Migration(ConsolidationDTOV2ToV3Mapper dtov2ToV3Migration) {
        this.dtov2ToV3Migration = dtov2ToV3Migration;
    }

    @Override
    public ConsolidationDetailsResponse forwardMap(ConsolidationDetailsV3Response consolidationDetailsV3Response) {
        return dtov2ToV3Migration.reverseMap(consolidationDetailsV3Response);
    }

    @Override
    public ConsolidationDetailsV3Response reverseMap(ConsolidationDetailsResponse consolidationDetailsResponse) {
        return dtov2ToV3Migration.forwardMap(consolidationDetailsResponse);
    }
}
