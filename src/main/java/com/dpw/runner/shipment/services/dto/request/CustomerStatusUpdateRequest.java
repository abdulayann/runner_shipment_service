package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import lombok.*;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CustomerStatusUpdateRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private BookingStatus bookingStatus;
}
