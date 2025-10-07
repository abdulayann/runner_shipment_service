package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ApiModel("Default Email Templates Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DefaultEmailTemplateRequest {
    private String module;
    private Long id;
    private Long emailTemplateId;
    private List<String> documentsList;

}
