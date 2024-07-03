package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
        private LocalDateTime updatedDate;
        private List<DateTimeChangeLogResponse> changeLogs;
    }
}
