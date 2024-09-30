package com.dpw.runner.booking.services.dto.v1.request;

import lombok.*;

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
