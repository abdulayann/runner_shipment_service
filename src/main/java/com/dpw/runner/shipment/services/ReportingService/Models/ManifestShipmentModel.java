package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import lombok.Data;

import java.util.List;

@Data
public class ManifestShipmentModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public ConsolidationModel consolidationDetails;
    public CarrierMasterData carrier;
    private List<ShipmentContainers> containers;
}
