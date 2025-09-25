package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
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
    @NotBlank(message = "Entity type can not be empty")
    private String entityType;
    @NotNull(message = "Entity Id can not be null")
    @Min(value = 1, message = "Entity id can not be zero/negative")
    private Long entityId;
    @NotBlank(message = "Entity Number can not be empty")
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
    private String internalEmails;
    private String externalEmails;
    @NotNull(message = "Requester can not be null")
    private PartiesRequest requester;
    @NotNull(message = "Shipper can not be null")
    private PartiesRequest shipper;
    private PartiesRequest consignee;
    @NotNull(message =  "Contract can not be null")
    private PartiesRequest contract;
    private PartiesRequest forwardingAgent;
    private PartiesRequest pickupFrom;
    private PartiesRequest deliveryTo;
    private SailingInformationRequest sailingInformation;
    private List<PartiesRequest> additionalParties;
    private List<CommonContainerRequest> containersList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
