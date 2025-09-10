package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingResponse implements IRunnerResponse {

    private Long id;
    private String status;
    private String bookingNo;
    private String carrierBookingNo;
    private String carrierBlNo;
    private String consolidationNo;
    private EntityType entityType;
    private Long entityId;
    private String entityNumber;
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
    private String bookingComment;
    private String carrierComment;
    private String internalEmails;
    private String externalEmails;
    private Map<String, Object> loadedContainerDropOffDetails;
    private Map<String, Object> emptyContainerPickupDetails;
    private PartiesResponse requester;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private PartiesResponse pickupFrom;
    private PartiesResponse deliveryTo;
    private SailingInformationResponse sailingInformation;
    private ShippingInstructionResponse shippingInstruction;
    private List<PartiesResponse> additionalParties;
    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
    private List<CarrierRoutingResponse> carrierRoutingList;
    private List<ContainerMisMatchWarning> containerMismatchWarningList;
}
