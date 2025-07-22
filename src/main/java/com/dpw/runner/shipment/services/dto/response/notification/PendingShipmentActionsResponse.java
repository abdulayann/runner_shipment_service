package com.dpw.runner.shipment.services.dto.response.notification;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
    private String consolidationNumber;
    private String masterBill;

    private String branch;
    private String branchDisplayName;

    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime ata;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime atd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime etd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime lat;

    private String requestedBy;
    private String requestedType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requestedOn;
    private Boolean hazardous;

    private String pol;
    private String pod;
}
