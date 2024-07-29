package com.dpw.runner.shipment.services.commons.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;

@Data
public class OrderContainerResponse implements Serializable {
    private String containerId;
    private String type;
    private Integer count;
    private String containerNumber;
}
