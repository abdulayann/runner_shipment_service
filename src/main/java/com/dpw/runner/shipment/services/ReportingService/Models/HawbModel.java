package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import lombok.Data;

import java.util.List;

@Data
public class HawbModel implements IDocumentModel{
    public ShipmentDetails shipmentDetails;
    public UsersDto usersDto;
    public Awb awb;
    private ConsolidationDetails consolidationDetails;
    private Awb mawb;
    private String entityType;
}
