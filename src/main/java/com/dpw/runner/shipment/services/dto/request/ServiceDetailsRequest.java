package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.Duration;
import java.util.Date;

@Getter
@Setter
@ApiModel("Service Details Request Model")
@ToString
public class ServiceDetailsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private Long consolidationId;
    private String serviceType;
    private Long contractorId;
    private Long contractorAddressId;
    private int srvLocation;
    private Date bookingDate;
    private String serviceCount;
    private Duration serviceDuration;
    private Date completionDate;
    private String refNumber;
    private String serviceNotes;
}
