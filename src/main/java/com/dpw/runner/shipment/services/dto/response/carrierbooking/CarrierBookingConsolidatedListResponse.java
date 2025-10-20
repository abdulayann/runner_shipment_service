package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierBookingConsolidatedListResponse implements IRunnerResponse {
    private List<IRunnerResponse> carrierBookingListResponses;
    private List<IRunnerResponse> verifiedGrossMassListResponses;
    private List<IRunnerResponse> shippingInstructionResponses;
}
