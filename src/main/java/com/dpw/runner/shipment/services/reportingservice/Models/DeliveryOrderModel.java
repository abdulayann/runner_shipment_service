package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Data;

import java.util.List;

@Data
public class DeliveryOrderModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    private List<ShipmentContainers> containers;
    public ConsolidationModel consolidationDetails;
    public UsersDto usersDto;
    public String paymentTerms;
    public String placeOfIssueName;
    public Hbl hbl;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    public TenantModel tenantModel;
    public Long transportInstructionId;
}
