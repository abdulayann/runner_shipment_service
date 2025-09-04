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
public class VerifiedGrossMassResponse implements IRunnerResponse {

    private Long id;
    private String status;
    private String carrierBookingNo;
    private String carrierBlNo;
    private String internalEmails;
    private String externalEmails;
    private PartiesResponse requestor;
    private PartiesResponse authorised;
    private PartiesResponse responsible;
    private SailingInformationResponse sailingInformation;
    private List<PartiesResponse> additionalParties;
    private List<CommonContainerResponse> containersList;
    private List<ReferenceNumberResponse> referenceNumbersList;
}
