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
public class CreateShipmentTaskRequest {
    private int tenantId;
    private int approverRole;
    private Object shipmentData;
    private List<Integer> tenantIds;
    private Boolean sendToOrganization;
    private String shipmentId;
    private String houseBill;
    private String masterBill;
    private long id;
}
