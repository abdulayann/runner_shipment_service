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
public class PendingConsolidationActionResponse implements IPendingActionsResponse {
    private Long shipmentId;
    private String shipmentNumber;
    private String houseBill;

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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime delivery;

    private String packs; // packs and packs unit
    private String weight; // weight and unit
    private String volume; // volume and unit
    private String chargeable; // chargeable wt and unit

    private String requestedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requestedOn;
    private String requestedType;

    private Boolean hazardous;

    private String pol;
    private String pod;
}
