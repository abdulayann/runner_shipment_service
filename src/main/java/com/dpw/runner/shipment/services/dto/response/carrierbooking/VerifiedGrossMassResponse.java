package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
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
public class VerifiedGrossMassResponse implements IRunnerResponse {

    private Long id;
    private String type = VerifiedGrossMassConstants.VERIFIED_GROSS_MASS_TYPE;
    private String serviceType;
    private CarrierBookingStatus bookingStatus;
    private VerifiedGrossMassStatus status;
    private ShippingInstructionStatus siStatus;
    private Boolean isDelegated;
    private Long entityId;
    private EntityType entityType;
    private String entityNumber;
    private String carrierBookingNo;
    private String carrierBlNo;
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;
    private PartiesResponse requestor;
    private PartiesResponse authorised;
    private PartiesResponse responsible;
    private SailingInformationResponse sailingInformation;
    private List<PartiesResponse> additionalParties;
    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
    private List<CommonContainerResponse> submittedContainersList;
    private List<VGMContainerWarningResponse> consolContainerWarningResponseList;
    private List<VGMContainerWarningResponse> vgmContainerWarningResponseList;
    private String createByUserEmail;
    private String submitByUserEmail;
    private String createdBy;
    private LocalDateTime createdAt;
    private String updatedBy;
    private LocalDateTime updatedAt;
}
