package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ShippingInstructionModel implements IDocumentModel {
    private TenantModel tenant;
    private ShipmentModel shipment;
    private List<ContainerModel> containersList;
}
