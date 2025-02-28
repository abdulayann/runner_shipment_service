package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidatedPackingListModel extends DocumentDataModel implements IDocumentModel {
    //TODO TenantRow alternative in service.
    private TenantModel tenant;
    private ConsolidationModel consolidationDetails;
}
