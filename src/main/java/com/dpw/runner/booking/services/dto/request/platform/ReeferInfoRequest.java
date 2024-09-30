package com.dpw.runner.booking.services.dto.request.platform;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder
public class ReeferInfoRequest implements Serializable {
    private Integer temperature;
    private Integer ventilation;
    private Integer humidity;

}
