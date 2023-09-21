package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ShippingInstructionModel implements IDocumentModel {
    private TenantModel tenant;
    private ShipmentDetails shipment;
    private List<Containers> containersList;
}
