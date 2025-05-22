package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs;

import lombok.Data;

import java.util.List;

@Data
public class RootPayload {
    private String fileName;
    private String agent;
    private String forwarder;
    private List<Job> jobs;
}
