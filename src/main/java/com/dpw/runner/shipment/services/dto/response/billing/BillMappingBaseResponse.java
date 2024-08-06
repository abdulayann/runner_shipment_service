package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BillMappingBaseResponse implements IRunnerResponse {

    private String id;
    private String orderNumber;
    private String moduleBill;
    private String houseBill;
    private String wayBill;
    private String masterBill;
    private String shipmentType;
    private String transportMode;
    private String jobType;
    private LocalDateTime etdDate;
    private LocalDateTime etaDate;
    private LocalDateTime moduleInsertDate;
    private String referenceNumber;
    private String shipper;
    private String consignee;
    private String moduleNumber;
    private String activityType;
    private String moduleStatus;
    private String containerType;
    private String origin;
    private String destination;
    private String carrier;
    private String vesselName;
    private String voyage;
}
