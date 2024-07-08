package com.dpw.runner.shipment.services.Kafka.Dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OrderManageDto {
    private OrderManagement payload;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class OrderManagement {
        private String moduleId;
        private String moduleGuid;
        private String orderManagementId;
        private String orderManagementNumber;
    }
}
