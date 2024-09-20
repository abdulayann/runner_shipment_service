package com.dpw.runner.shipment.services.dto.request;

import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
public class UpstreamDateUpdateRequest implements Serializable {

    private DateAndSource ata;
    private DateAndSource atd;
    private DateAndSource eta;
    private DateAndSource etd;

    @Data
    public static class DateAndSource implements Serializable {
        private LocalDateTime dateTime;
        private String source;
    }
}
