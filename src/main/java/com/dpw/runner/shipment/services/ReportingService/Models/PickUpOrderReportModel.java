package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;

import java.util.List;

public class PickUpOrderReportModel implements IDocumentModel {
    public PartiesModel pickUpTransportAddress;
    public HblModel hblModel;
    public List<TILegsModel> legs;
}
