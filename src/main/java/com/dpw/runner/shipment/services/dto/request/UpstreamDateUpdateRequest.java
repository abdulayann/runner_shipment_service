package com.dpw.runner.shipment.services.dto.request;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class UpstreamDateUpdateRequest {

    private DateAndSource ata;
    private DateAndSource atd;
    private DateAndSource eta;
    private DateAndSource etd;

    @Data
    public static class DateAndSource {
        private LocalDateTime dateTime;
        private String source;
    }
}
