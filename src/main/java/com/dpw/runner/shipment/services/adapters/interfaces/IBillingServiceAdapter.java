package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

public interface IBillingServiceAdapter {

    Boolean fetchActiveInvoices(CommonGetRequest commonGetRequest) throws RunnerException;

    List<BillingSummary> fetchBillingBulkSummaryBranchWise(BillingBulkSummaryBranchWiseRequest request);

    List<BillingDueSummary> fetchBillingDueSummary(BillingBulkSummaryBranchWiseRequest request);

    List<BillChargesBaseResponse> fetchBillCharges(BillChargesFilterRequest request);

    BillBaseResponse fetchBill(BillRetrieveRequest request);

    List<ChargeTypeBaseResponse> fetchChargeTypes(ChargeTypeFilterRequest request);

    ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest, HttpHeaders headers);

    List<BillingSummary> fetchBillingBulkSummary(BillingBulkSummaryRequest request);

    ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
                                                       ShipmentDetailsResponse shipmentDetailsResponse, HttpHeaders headers);

    ShipmentBillingListResponse fetchShipmentBillingData(ShipmentBillingListRequest request);

    LocalDateTime fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest request);
}
