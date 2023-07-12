package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.Duration;
import java.time.LocalDateTime;

@Getter
@Setter
@ApiModel("Service Details Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ServiceDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private Long consolidationId;
    private String serviceType;
    private PartiesRequest contractor;
    private int srvLocation;
    private LocalDateTime bookingDate;
    private Long serviceCount;
    private Duration serviceDuration;
    private LocalDateTime completionDate;
    private String refNumber;
    private String serviceNotes;
}
