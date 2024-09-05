package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.billing.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.*;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.List;

public interface IBillingServiceAdapter {

    Boolean fetchActiveInvoices(CommonGetRequest commonGetRequest) throws RunnerException;

    List<BillingSummary> fetchBillingBulkSummaryBranchWise(BillingBulkSummaryBranchWiseRequest request);

    List<BillChargesBaseResponse> fetchBillCharges(BillChargesFilterRequest request);

    BillBaseResponse fetchBill(BillRetrieveRequest request);

    List<ChargeTypeBaseResponse> fetchChargeTypes(ChargeTypeFilterRequest request);

    ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest);

    ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse);

    List<BillingSummary> fetchBillingBulkSummary(BillingBulkSummaryRequest request);

    ShipmentBillingListResponse fetchShipmentBillingData(ShipmentBillingListRequest request);

    LocalDateTime fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest request);
}
