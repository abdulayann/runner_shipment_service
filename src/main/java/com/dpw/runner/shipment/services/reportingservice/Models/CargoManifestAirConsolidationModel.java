package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

import java.util.List;

@Data
public class CargoManifestAirConsolidationModel implements IDocumentModel{
    private ConsolidationModel consolidationModel;
    private List<ShipmentModel> shipmentModelList;
    private List<Awb> awbList;
    private TenantModel tenantModel;
    private PackSummaryResponse packSummaryResponse;
}
