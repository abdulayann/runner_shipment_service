package com.dpw.runner.shipment.services.dto.response.notification;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PendingShipmentActionsResponse implements IPendingActionsResponse {
    private Long consolId;

    private String branch;

    private LocalDateTime ata;
    private LocalDateTime atd;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime lat;

    private String requestedBy;
    private LocalDateTime requestedOn;
    private Boolean hazardous;
}
