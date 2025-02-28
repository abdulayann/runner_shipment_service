package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsolidationManifestPrintModel implements IDocumentModel {
    private List<ShipmentModel> shipments;
    private ConsolidationModel consol;
}
