package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.TruckDriverDetailsModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class TruckDriverModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
    private List<ShipmentContainers> containers;
    public ConsolidationModel consolidationDetails;
    public TenantModel tenant;
    public long totalPacks;
    public BigDecimal totalWeight;
    private List<TruckDriverDetailsModel> truckDriverDetails;
}
