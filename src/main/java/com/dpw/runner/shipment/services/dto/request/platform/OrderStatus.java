package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.Data;

@Data
public class OrderStatus {
    private int statusCode;
    private String errorMessage;
}
