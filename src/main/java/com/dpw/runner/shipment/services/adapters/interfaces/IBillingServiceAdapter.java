package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.time.LocalDateTime;

public interface IBillingServiceAdapter {

    Boolean fetchActiveInvoices(CommonGetRequest commonGetRequest) throws RunnerException;

    LocalDateTime fetchLastPostedInvoiceDate(InvoiceSummaryRequest invoiceSummaryRequest);
}
