package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@ApiModel("Booking Carriage Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BookingCarriageRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private String portOfLoading;
    private String portOfDischarge;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private String vessel;
    private String voyage;
    private String carriageType;
    private String carriageMode;
}
