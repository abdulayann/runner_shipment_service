package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

@Data
public class HawbModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
    public Awb awb;
    private ConsolidationModel consolidationDetails;
    private Awb mawb;
    private String entityType;
}
