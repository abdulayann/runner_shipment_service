package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingRequest implements Serializable {
    private Long id;
    private String status;
    private String bookingNo;
    private String carrierBookingNo;
    private String carrierBlNo;
    private String entityType;
    private Long entityId;
    private String entityNumber;
    @NotBlank(message = "Service type can not be empty")
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
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;
    private PartiesRequest requester;
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    private PartiesRequest contract;
    private PartiesRequest forwardingAgent;
    private PartiesRequest pickupFrom;
    private PartiesRequest deliveryTo;
    private SailingInformationRequest sailingInformation;
    private List<PartiesRequest> additionalParties;
    private List<CommonContainerRequest> containersList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
