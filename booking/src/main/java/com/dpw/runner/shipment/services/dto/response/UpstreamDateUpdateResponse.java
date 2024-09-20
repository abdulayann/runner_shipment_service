package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Builder;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class UpstreamDateUpdateResponse implements IRunnerResponse {

    private DateAndLogResponse ata;
    private DateAndLogResponse atd;
    private DateAndLogResponse eta;
    private DateAndLogResponse etd;


    @Data
    @Builder
    public static class DateAndLogResponse implements Serializable {
        @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
        private LocalDateTime updatedDate;
        private List<DateTimeChangeLogResponse> changeLogs;
    }
}
