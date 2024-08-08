package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.http.ResponseEntity;

public interface IBillingServiceAdapter {

    Boolean fetchActiveInvoices(CommonGetRequest commonGetRequest) throws RunnerException;

    List<BillChargesBaseResponse> fetchBillCharges(BillChargesFilterRequest request);

    BillBaseResponse fetchBill(BillRetrieveRequest request);

    List<ChargeTypeBaseResponse> fetchChargeTypes(ChargeTypeFilterRequest request);

    ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest);

    ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse);

    LocalDateTime fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest request);
}
