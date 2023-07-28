package com.dpw.runner.shipment.services.dto.v1.request;

import lombok.*;

import java.util.List;


@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class SendConsolidationRequest {
    private int tenantId;
    private int approverRole;
    private Object consoleRow;
    private List<Integer> tenantIds;
    private boolean sendToOrganization;
    private String mawb;
    private String consolidationNumber;
    private List<String> houseBill;
    private List<String> shipmentIds;
    private long id;
}
