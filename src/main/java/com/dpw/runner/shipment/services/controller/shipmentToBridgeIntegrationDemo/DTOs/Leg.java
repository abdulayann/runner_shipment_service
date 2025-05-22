package com.dpw.runner.shipment.services.controller.shipmentToBridgeIntegrationDemo.DTOs;

import lombok.Data;

@Data
public class Leg {
    private String ata;
    private String atd;
    private Carrier carrier;
    private String clsCutOffDateTime;
    private String ensCutOffDateTime;
    private String eta;
    private String etd;
    private String loop;
    private Port pod;
    private Port pol;
    private Vessel vessel;
    private String voyage;
}
