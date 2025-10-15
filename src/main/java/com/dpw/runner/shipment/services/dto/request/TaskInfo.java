package com.dpw.runner.shipment.services.dto.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonIncludeProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class TaskInfo {
    private String transportMode;
    private String shipmentType;
    private String origin;
    private String destination;
    private String masterBill;
    private String houseBill;
    private String shipmentId;
    private String consolidationNumber;
}
