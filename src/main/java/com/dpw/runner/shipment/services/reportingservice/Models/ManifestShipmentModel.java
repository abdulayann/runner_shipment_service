package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import lombok.Data;

import java.util.List;

@Data
public class ManifestShipmentModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public ConsolidationModel consolidationDetails;
    public CarrierMasterData carrier;
    private List<ShipmentContainers> containers;
}
