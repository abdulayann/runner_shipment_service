package com.dpw.runner.shipment.services.ReportingService;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReportsFactory {
    @Autowired
    private ArrivalNoticeReport arrivalNoticeReport;
    @Autowired
    private BookingConfirmationReport bookingConfirmationReport;
    @Autowired
    private DeliveryOrderReport deliveryOrderReport;
    @Autowired
    private HblReport hblReport;
    @Autowired
    private HawbReport hawbReport;
    @Autowired
    private MawbReport mawbReport;
    @Autowired
    private CargoManifestReport cargoManifestReport;
    @Autowired
    private CommercialInvoiceReport commercialInvoiceReport;
    @Autowired
    private ConsolidatedPackingListReport consolidatedPackingListReport;

    @Autowired
    private FreightCertificationReport freightCertificationReport;

    public IReport getReport(String key)
    {
        switch(key)
        {
            case ReportConstants.ARRIVAL_NOTICE:
                return arrivalNoticeReport;
            case ReportConstants.BOOKING_CONFIRMATION:
                return bookingConfirmationReport;
            case ReportConstants.DELIVERY_ORDER:
                return deliveryOrderReport;
            case ReportConstants.HOUSE_BILL:
                return hblReport;
            case ReportConstants.HAWB:
                return hawbReport;
            case ReportConstants.MAWB:
                return mawbReport;
            case ReportConstants.CARGO_MANIFEST:
                return cargoManifestReport;
            case ReportConstants.COMMERCIAL_INVOICE:
                return commercialInvoiceReport;
            case ReportConstants.CONSOLIDATED_PACKING_LIST:
                return consolidatedPackingListReport;
            case ReportConstants.FREIGHT_CERTIFICATION:
                return freightCertificationReport;
        }
        return null;
    }
}
