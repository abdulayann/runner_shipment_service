package com.dpw.runner.shipment.services.kafka.dto;

import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
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
        private BookingStatus moduleStatus;
        private String moduleId;
        private String moduleGuid;
        private String orderManagementId;
        private String orderManagementNumber;
        private Integer tenantId;
    }
}
