package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VerifiedGrossMassInttraResponse implements IRunnerResponse {

    private UUID messageGuid;
    private LocalDateTime messageDateTime;
    private String tenantId;
    private String state;
    private CommonContainerResponse container;
    private String submitterReference;
    // Fetch remoteId (as partyId), remoteIdType (as partyIdType), fullName (as partyName) from orgData
    private PartiesResponse requestor;
    private PartiesResponse authorised;
    private PartiesResponse responsible;
    private NotificationContactResponse requestorNotificationContact;
    private String carrierBookingNo;
    private String carrierScacCode;
    private String carrierDescription;
    private NotificationContactResponse carrierNotificationContact;
    private boolean isDelegated;
    private String fileName;

}
