package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
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
