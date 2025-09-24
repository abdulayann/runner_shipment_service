package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
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
    @NotNull(message = "Entity type can not be empty")
    private EntityType entityType;
    @NotNull(message = "Entity Id can not be null")
    @Min(value = 1, message = "Entity id can not be zero/negative")
    private Long entityId;
    @NotBlank(message = "Entity Number can not be empty")
    private String entityNumber;
    private String internalEmails;
    private String externalEmails;
    @NotNull(message = "Requester can not be null")
    private PartiesRequest requestor;
    private PartiesRequest authorised;
    private PartiesRequest responsible;
    private SailingInformationRequest sailingInformation;
    private List<PartiesRequest> additionalParties;
    private List<CommonContainerRequest> containersList;
    private List<ReferenceNumberRequest> referenceNumbersList;
}
