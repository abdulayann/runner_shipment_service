package com.dpw.runner.shipment.services.dto.patchrequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.openapitools.jackson.nullable.JsonNullable;

import java.time.LocalDateTime;

@Getter
@Setter
@ApiModel("Booking Carriage Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BookingCarriagePatchRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private JsonNullable<Long> bookingId;
    private JsonNullable<Long> shipmentId;
    private JsonNullable<Long> vesselId;
    private JsonNullable<Long> polId;
    private JsonNullable<Long> podId;
    private JsonNullable<LocalDateTime> eta;
    private JsonNullable<LocalDateTime> etd;
    private JsonNullable<String> vessel;
    private JsonNullable<String> voyage;
    private JsonNullable<String> carriageType;
    private JsonNullable<String> carriageMode;
}
