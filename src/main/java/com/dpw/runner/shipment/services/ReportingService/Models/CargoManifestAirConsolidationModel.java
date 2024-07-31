package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.commons.entity.Awb;
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
