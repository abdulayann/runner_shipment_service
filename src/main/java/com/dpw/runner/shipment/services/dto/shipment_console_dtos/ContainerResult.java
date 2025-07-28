package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerResult {
    private Integer containerCount = 0;
    private Integer dgContCount = 0;
    private BigDecimal teuCount = BigDecimal.ZERO;
    private BigDecimal totalWeight = BigDecimal.ZERO;

    public ContainerResult(Integer containerCount, Integer dgContCount, BigDecimal teuCount) {
        this.containerCount = containerCount;
        this.dgContCount = dgContCount;
        this.teuCount = teuCount;
    }
}

