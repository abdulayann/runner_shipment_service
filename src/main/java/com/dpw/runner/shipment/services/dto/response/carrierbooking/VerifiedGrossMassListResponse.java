package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VerifiedGrossMassListResponse implements IRunnerResponse {
    private Long id;  // from MultiTenancy / BaseEntity
    private String type = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_TYPE;
    private String status;
    private Integer tenantId;
    private String carrierBookingNo;
    private String carrierBlNo;
    private Boolean isDelegated;
    private String entityType;
    private String entityNumber;
    private Long entityId;
    // Relations
    private PartiesResponse requester;
    private PartiesResponse authorised;
    private PartiesResponse responsible;
    private List<PartiesResponse> additionalParties;

    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;

    private SailingInformationResponse sailingInformation;

    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
    private String createdBy;
    private LocalDateTime createdAt;
    private String updatedBy;
    private LocalDateTime updatedAt;
    private String createByUserEmail;
    private String submitByUserEmail;
}
