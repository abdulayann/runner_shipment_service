package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Email Templates Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EmailTemplatesRequest extends CommonRequest implements IRunnerRequest {

    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("Type")
    private String type;
    @JsonProperty("Body")
    private String body;
    @JsonProperty("Subject")
    private String subject;
    @JsonProperty("ShipmentSettingsId")
    private Long shipmentSettingsId;
}
