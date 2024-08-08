package com.dpw.runner.shipment.services.adapters.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

public interface IBillingServiceAdapter {

    Boolean fetchActiveInvoices(CommonGetRequest commonGetRequest) throws RunnerException;

    List<BillChargesBaseResponse> fetchBillCharges(BillChargesFilterRequest request);

    BillBaseResponse fetchBill(BillRetrieveRequest request);

    List<ChargeTypeBaseResponse> fetchChargeTypes(ChargeTypeFilterRequest request);

    ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest);

    ExternalBillPayloadRequest getBillCreationRequest(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse);
}
