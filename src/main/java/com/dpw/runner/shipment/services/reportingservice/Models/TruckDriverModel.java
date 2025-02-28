package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.TruckDriverDetailsModel;
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
