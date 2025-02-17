package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Data;

import java.util.List;

@Data
public class DeliveryOrderModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public ConsolidationModel consolidationDetails;
    public UsersDto usersDto;
    public String paymentTerms;
    public String placeOfIssueName;
    public Hbl hbl;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    public TenantModel tenantModel;
    public Long transportInstructionId;
    private List<ShipmentContainers> containers;
}
