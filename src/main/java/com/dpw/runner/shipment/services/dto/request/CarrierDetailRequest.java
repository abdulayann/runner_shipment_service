package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;


@Data
@Builder
@ApiModel("Carrier Details Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CarrierDetailRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private String shippingLine;
    private String vessel;
    private String voyage;
    private String origin;
    private String destination;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;
    private Long shipmentId;
}