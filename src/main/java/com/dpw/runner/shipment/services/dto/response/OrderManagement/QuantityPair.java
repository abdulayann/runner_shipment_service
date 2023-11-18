package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class QuantityPair {
    private BigDecimal amount;
    private String unit;
}
