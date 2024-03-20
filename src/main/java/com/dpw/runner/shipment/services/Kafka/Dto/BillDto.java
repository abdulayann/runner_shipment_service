package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @Builder
@AllArgsConstructor @NoArgsConstructor
public class BillDto {
    private String moduleId;
    private String moduleTypeCode;
    private String jobStatus;
}
