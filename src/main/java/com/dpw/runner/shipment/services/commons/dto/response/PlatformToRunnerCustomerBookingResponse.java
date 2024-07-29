package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("Customer Booking Response Model for platform")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class PlatformToRunnerCustomerBookingResponse implements IRunnerResponse {
    private String bookingNumber;
    private List<ReferenceNumbersGuidMapResponse> containers;
    private List<ReferenceNumbersGuidMapResponse> packs;
    private List<ReferenceNumbersGuidMapResponse> charges;
    private List<ReferenceNumbersGuidMapResponse> routings;

    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @Data
    public static class ReferenceNumbersGuidMapResponse implements IRunnerResponse {
        private String reference_id;
        private UUID guid;
    }
}
