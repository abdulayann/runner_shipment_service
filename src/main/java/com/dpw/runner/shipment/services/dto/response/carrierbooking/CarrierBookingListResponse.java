package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingListResponse implements IRunnerResponse {
    private Long id;  // from MultiTenancy / BaseEntity
    private String status;
    private String bookingNo;
    private Integer tenantId;
    private String carrierBookingNo;
    private String mblNo;
    private String consolidationNo;
    private String serviceType;
    private String bookingOffice;
    private String bookingComment;
    private String carrierComment;

    // Relations
    private PartiesResponse requester;
    private PartiesResponse shipper;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private List<PartiesResponse> additionalParties;
    private PartiesResponse pickupFrom;
    private PartiesResponse deliveryTo;

    private String internalEmails;
    private String externalEmails;

    private ShippingInstructionResponse shippingInstruction;
    private SailingInformationResponse sailingInformation;

    private List<CommonContainerResponse> containersList;
    private List<CarrierRoutingResponse> carrierRoutingList;
    private List<ReferenceNumberResponse> referenceNumbersList;
}
