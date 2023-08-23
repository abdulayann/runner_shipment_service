package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.entity.ReferenceNumbers;

import java.util.List;

public class BookingConfirmationModel extends HblModel implements IDocumentModel{
    public List<ReferenceNumbers> referenceNumbersList;
    public String polName;
    public String polCountry;
    public String podName;
    public String podCountry;
}
