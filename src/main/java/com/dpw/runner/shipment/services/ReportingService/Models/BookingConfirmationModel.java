package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import lombok.Data;

import java.util.List;

@Data
public class BookingConfirmationModel implements IDocumentModel {
    public HblModel hblModel;
    private List<ReferenceNumbersModel> referenceNumbersList;
}
