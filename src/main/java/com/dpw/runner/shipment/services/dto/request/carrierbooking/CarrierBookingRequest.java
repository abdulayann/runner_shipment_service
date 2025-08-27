package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingRequest {

    private String status;
    private String bookingNo;
    private String carrierBookingNo;
    private String mblNo;
    private String consolidationNo;
    private String serviceType;

    @UnlocationData
    private String bookingOffice;

    private String bookingComment;
    private String carrierComment;

    private PartiesRequest requester;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest forwardingAgent;

    private List<PartiesRequest> additionalParties;
    private PartiesRequest pickupFrom;
    private PartiesRequest deliveryTo;

    private String internalEmails;
    private String externalEmails;

    private Long shippingInstructionId;
    private Long sailingInformationId;

    // Children (optional full details)
    private List<CommonContainerRequest> containersList;
    private List<CarrierRoutingRequest> carrierRoutingList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
