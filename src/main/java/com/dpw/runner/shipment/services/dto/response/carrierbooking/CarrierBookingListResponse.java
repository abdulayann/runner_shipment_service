package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class CarrierBookingListResponse implements IRunnerResponse {
    private List<CarrierBookingResponse> carrierBookingResponseList;
    @JsonIgnore
    private Integer totalPages;
    @JsonIgnore
    private Long numberOfRecords;
}
