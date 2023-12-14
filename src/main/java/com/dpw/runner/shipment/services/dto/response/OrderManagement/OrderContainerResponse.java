package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import lombok.Data;

@Data
public class OrderContainerResponse {
    private String containerId;
    private String type;
    private Integer count;
    private String containerNumber;
}
