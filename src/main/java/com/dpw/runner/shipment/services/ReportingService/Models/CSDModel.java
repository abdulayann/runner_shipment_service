package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class CSDModel implements IDocumentModel {
    private ConsolidationModel consolidationModel;
    private ShipmentModel shipmentModel;
    private UsersDto usersDto;
    private Awb awb;
}
