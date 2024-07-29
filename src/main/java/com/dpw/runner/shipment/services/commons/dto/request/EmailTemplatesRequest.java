package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
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

    private Long id;
    private String name;
    private String type;
    private String body;
    private String subject;
    private Long shipmentSettingsId;
}
