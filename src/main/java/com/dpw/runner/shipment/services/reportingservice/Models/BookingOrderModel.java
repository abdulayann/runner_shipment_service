package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import lombok.Data;

@Data
public class BookingOrderModel implements IDocumentModel {
    private ShipmentModel shipmentModel;
    private UsersDto user;
    private TenantModel tenantModel;
}
