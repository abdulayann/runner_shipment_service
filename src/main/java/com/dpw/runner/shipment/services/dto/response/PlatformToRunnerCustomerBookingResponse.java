package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
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
    public static class ReferenceNumbersGuidMapResponse {
        private String reference_id;
        private UUID guid;
    }
}
