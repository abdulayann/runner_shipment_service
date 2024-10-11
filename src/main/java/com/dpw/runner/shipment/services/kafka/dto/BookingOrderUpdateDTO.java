package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BookingOrderUpdateDTO
{
    private String event;
    private BookingLinkDelinkOrder data;
    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class BookingLinkDelinkOrder {
        private String orderId;
        private String orderNumber;
        private Long tenantId;
        private String bookingId;
    }
}

