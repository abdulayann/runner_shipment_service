package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.config.CustomLocalTimeDeserializer;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.time.LocalTime;

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
    private String srvLocation;
    private LocalDateTime bookingDate;
    private Long serviceCount;
    @JsonDeserialize(using = CustomLocalTimeDeserializer.class)
    private LocalTime serviceDuration;
    private LocalDateTime completionDate;
    private String refNumber;
    private String serviceNotes;
}
