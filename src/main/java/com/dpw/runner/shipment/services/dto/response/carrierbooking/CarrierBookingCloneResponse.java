package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingCloneResponse implements IRunnerResponse {

    private String status;
    private EntityType entityType;
    private Long entityId;
    private String entityNumber;
    private String serviceType;
    private String bookingOffice;
    private String bookingComment;
    private String internalEmails;
    private String externalEmails;
    private String createByUserEmail;
    private String submitByUserEmail;
    private PartiesResponse requester;
    private PartiesResponse shipper;
    private PartiesResponse contract;
    private PartiesResponse consignee;
    private PartiesResponse forwardingAgent;
    private SailingInformationResponse sailingInformation;
    private List<PartiesResponse> additionalParties;
    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
}
