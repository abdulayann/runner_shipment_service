package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VerifiedGrossMassRequest implements Serializable {
    private Long id;
    private String serviceType;
    private VerifiedGrossMassStatus status;
    private String carrierBookingNo;
    private String carrierBlNo;
    private Boolean isDelegated;
    private EntityType entityType;
    private Long entityId;
    private String entityNumber;
    private List<String> internalEmailsList;
    private List<String> externalEmailsList;
    private String otherInternalEmails;
    private String otherExternalEmails;
    private PartiesRequest requestor;
    private PartiesRequest authorised;
    private PartiesRequest responsible;
    private SailingInformationRequest sailingInformation;
    private List<PartiesRequest> additionalParties;
    private List<CommonContainerRequest> containersList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
