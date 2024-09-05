package com.dpw.runner.shipment.services.dto.v1.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;


@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class CreateConsolidationTaskRequest {
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
