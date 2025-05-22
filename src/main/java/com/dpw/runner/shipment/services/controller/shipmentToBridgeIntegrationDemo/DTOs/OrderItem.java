package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs;

import lombok.Data;

@Data
public class OrderItem {
    private String orderNumber;
    private int cartons;
    private double cbm;
    private int quantity;
    private double weight;
    private String itemCode;
    private String description;
    private String colour;
    private String size;
}
