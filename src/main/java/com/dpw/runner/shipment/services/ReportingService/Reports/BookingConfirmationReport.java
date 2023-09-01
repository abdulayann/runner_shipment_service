package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class BookingConfirmationReport extends IReport{

    @Override
    public Map<String, Object> getData(Long id) {
        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) getDocumentModel(id);
        return populateDictionary(bookingConfirmationModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        return null;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        return null;
    }
}
