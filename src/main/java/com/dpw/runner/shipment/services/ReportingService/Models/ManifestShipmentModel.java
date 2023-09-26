package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;

import java.util.List;

public class ManifestShipmentModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public ConsolidationModel consolidationDetails;
    public CarrierMasterData carrier;
    public List<ShipmentContainers> containers;
}
