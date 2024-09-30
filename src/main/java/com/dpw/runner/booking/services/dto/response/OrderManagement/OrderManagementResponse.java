package com.dpw.runner.booking.services.dto.response.OrderManagement;

import lombok.Data;

@Data
public class OrderManagementResponse {
    private Object status;
    private OrderManagementDTO order;
}
