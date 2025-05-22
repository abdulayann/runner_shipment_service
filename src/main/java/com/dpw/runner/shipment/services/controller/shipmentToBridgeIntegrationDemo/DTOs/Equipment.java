package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs;

import lombok.Data;

import java.util.List;

@Data
public class Equipment {
    private String containerNumber;
    private String seal;
    private double tareWeight;
    private String type;
    private List<OrderItem> orderItems;
}
