package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
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
    private String bookingOffice;

    @Size(max = 10000, message = "Booking Comment cannot exceed 10,000 characters")
    private String bookingComment;
    @Size(max = 10000, message = "Carrier Comment cannot exceed 10,000 characters")
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

    private List<CommonContainerRequest> containersList;
    private List<CarrierRoutingRequest> carrierRoutingList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
