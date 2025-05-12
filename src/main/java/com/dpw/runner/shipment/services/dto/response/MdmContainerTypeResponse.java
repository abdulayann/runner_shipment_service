package com.dpw.runner.shipment.services.dto.response;

import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MdmContainerTypeResponse {
    private long id;
    private UUID uuid;
    private String code;
    private BigDecimal teu;
    private String description;
}
