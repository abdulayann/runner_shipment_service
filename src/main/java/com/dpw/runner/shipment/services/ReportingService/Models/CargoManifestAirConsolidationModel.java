package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.Awb;

import java.util.List;

public class CargoManifestAirConsolidationModel implements IDocumentModel{
    public ConsolidationModel consolidationModel;
    public List<ShipmentModel> shipmentModelList;
    public List<Awb> awbList;
    public TenantModel tenantModel;
    public PackSummaryResponse packSummaryResponse;
}
