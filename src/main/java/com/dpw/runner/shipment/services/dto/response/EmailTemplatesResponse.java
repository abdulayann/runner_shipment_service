package com.dpw.runner.shipment.services.dto.response;

import lombok.Data;

@Data
public class EmailTemplatesResponse {

    private Long id;
    private String name;
    private String type;
    private String body;
    private String subject;
    private Long shipmentSettingsId;
}
