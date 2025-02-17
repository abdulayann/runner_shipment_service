package com.dpw.runner.shipment.services.kafka.dto;

import com.dpw.runner.shipment.services.entity.enums.FileStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BillDto {
    private Bill payload;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Bill {
        private String moduleId;
        private String moduleTypeCode;
        private String jobStatus;
        private FileStatus fileStatus;
    }
}

