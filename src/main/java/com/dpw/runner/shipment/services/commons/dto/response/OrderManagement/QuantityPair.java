package com.dpw.runner.shipment.services.commons.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
public class QuantityPair implements Serializable {
    private BigDecimal amount;
    private String unit;
}
