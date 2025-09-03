package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Builder
public class CustomerBookingV3ListResponse implements IRunnerResponse {
    private List<CustomerBookingV3Response> customerBookingV3Responses = new ArrayList<>();
    private Integer totalPages;
    private Long totalCount;
}
