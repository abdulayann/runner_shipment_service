package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AwbOtherInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String shipper;
    private String carrier;
    private String executedAt;
    private LocalDateTime executedOn;
    private String carrierName;
    private String carrierHqAddress;
    private String legalCompanyName;
    private String address1;
    private String address2;
    private String city;
    private String state;
    private String pincode;
    private String countryCode;
    private String countryName;
    private String branch;
}
