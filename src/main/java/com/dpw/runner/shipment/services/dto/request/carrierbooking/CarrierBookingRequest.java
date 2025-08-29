package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingRequest {
    private Long id;
    private String status;
    private String bookingNo;
    private String carrierBookingNo;
    private String carrierBlNo;
    private String consolidationNo;
    private String serviceType;
    private String bookingOffice;
    private LocalDateTime pickupFromReqEmptyPositioningDate;
    private LocalDateTime pickupFromReqFullPickupDate;
    private String pickupFromContactName;
    private String pickupFromContactNo;
    private LocalDateTime deliveryToReqEmptyPositioningDate;
    private LocalDateTime deliveryToReqFullPickupDate;
    private String deliveryToContactName;
    private String deliveryToContactNo;
    @Size(max = 10000, message = "Booking Comment cannot exceed 10,000 characters")
    private String bookingComment;
    @Size(max = 10000, message = "Carrier Comment cannot exceed 10,000 characters")
    private String carrierComment;
    private String internalEmails;
    private String externalEmails;
    private PartiesRequest requester;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest forwardingAgent;
    private PartiesRequest pickupFrom;
    private PartiesRequest deliveryTo;
    private SailingInformationRequest sailingInformationRequest;
    private List<PartiesRequest> additionalParties;
    private List<CommonContainerRequest> containersList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
