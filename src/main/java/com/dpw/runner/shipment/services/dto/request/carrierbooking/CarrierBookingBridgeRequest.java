package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CarrierBookingBridgeRequest extends CarrierBookingResponse implements IRunnerRequest {
    private String fileName;
    private String carrierScacCode;
    private String state;
    private String carrierDescription;
}
