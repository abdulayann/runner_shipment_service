package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Date;

@Getter
@Setter
@ApiModel("Booking Carriage Request Model")
@ToString
public class BookingCarriageRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long bookingId;
    private Long shipmentId;
    private Long vesselId;
    private Long polId;
    private Long podId;
    private Date eta;
    private Date etd;
    private String vessel;
    private String voyage;
    private String carriageType;
    private String carriageMode;
}
