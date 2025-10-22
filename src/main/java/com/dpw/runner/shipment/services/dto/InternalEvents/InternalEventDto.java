package com.dpw.runner.shipment.services.dto.InternalEvents;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InternalEventDto {

    private Long id;
    private Long entityId;
    private String entityType;
    private String publishedStatus;
    private LocalDateTime publishedTimestamp;
    private String consumedStatus;
    private LocalDateTime consumedTimestamp;
}

