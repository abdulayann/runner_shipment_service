package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import lombok.Data;

import java.util.List;

@Data
public class BookingConfirmationModel extends HblModel implements IDocumentModel{
    private List<ReferenceNumbersModel> referenceNumbersList;
    public String polName;
    public String polCountry;
    public String podName;
}
