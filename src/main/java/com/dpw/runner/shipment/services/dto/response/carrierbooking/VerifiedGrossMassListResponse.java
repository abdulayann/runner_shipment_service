package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class VerifiedGrossMassListResponse implements IRunnerResponse {
    private Long id;  // from MultiTenancy / BaseEntity
    private String status;
    private Integer tenantId;
    private String carrierBookingNo;
    // Relations
    private PartiesResponse requester;
    private PartiesResponse authorised;
    private PartiesResponse responsible;
    private List<PartiesResponse> additionalParties;

    private String internalEmails;
    private String externalEmails;

    private SailingInformationResponse sailingInformation;

    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
}
