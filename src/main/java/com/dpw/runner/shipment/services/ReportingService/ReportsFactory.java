package com.dpw.runner.shipment.services.ReportingService;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import org.springframework.beans.factory.annotation.Autowired;


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
            case ReportConstants.HBL:
                return hblReport;
            case ReportConstants.HAWB:
                return hawbReport;
            case ReportConstants.MAWB:
                return mawbReport;
        }
        return null;
    }
}
