package com.dpw.runner.booking.services.adapters.interfaces;

import com.dpw.runner.booking.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.booking.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.booking.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.booking.services.entity.CustomerBooking;
import org.springframework.http.ResponseEntity;

public interface IBillingServiceAdapter {

    ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest);

    ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse);
}
