package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class EmailTemplatesResponse implements IRunnerResponse {

    private Long id;
    private String name;
    private String type;
    private String body;
    private String subject;
    private Long shipmentSettingsId;
}
