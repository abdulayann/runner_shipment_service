package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Email Templates Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EmailTemplatesRequest {

    private Long id;
    private String name;
    private String type;
    private String body;
    private String subject;
    private Long shipmentSettingsId;
}
