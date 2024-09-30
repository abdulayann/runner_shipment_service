package com.dpw.runner.booking.services.dto.request.platform;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AirCarrierDetailsRequest implements IRunnerRequest {
    private String carrier_name;
    private String airline_number;
}
