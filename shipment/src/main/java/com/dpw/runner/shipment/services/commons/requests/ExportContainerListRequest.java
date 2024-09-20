package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

@Data
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ExportContainerListRequest {
    private String consolidationId;
    private String entity;
    private Long freeTimeNoOfDaysStorage;
    private Long freeTimeNoOfDaysDetention;
    private Boolean isShipmentContainer;
}
