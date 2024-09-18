package com.dpw.runner.shipment.services.entity.fnm;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class FNMResponseStatus {

    @JacksonXmlProperty(localName = "ConditionCode")
    @Size(max = 15, message = "conditionCode exceeds the maximum size")
    @NotBlank(message = "conditionCode is required")
    private String conditionCode;

    @JacksonXmlProperty(localName = "ReasonCode")
    @Size(max = 15, message = "reasonCode exceeds the maximum size")
    private String reasonCode;

    @JacksonXmlProperty(localName = "Reason")
    @Size(max = 70, message = "reason exceeds the maximum size")
    @NotBlank(message = "reason is required")
    private String reason;

    @JacksonXmlProperty(localName = "Information")
    @Size(max = 70, message = "information exceeds the maximum size")
    private String information;
}
