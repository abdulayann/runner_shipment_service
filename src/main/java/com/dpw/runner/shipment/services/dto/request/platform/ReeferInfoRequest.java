package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ReeferInfoRequest {
    private Integer temperature;
    private Integer ventilation;
    private Integer humidity;

}
