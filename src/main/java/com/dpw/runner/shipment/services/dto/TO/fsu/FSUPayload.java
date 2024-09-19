package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JacksonXmlRootElement(localName = "StatusMessage")
public class FSUPayload {
    @Valid
    @JacksonXmlProperty(localName = "MessageHeaderDocument")
    @NotNull(message = "Business header document cannot be null")
    private FSUMessageHeaderDocument messageHeaderDocument;

    @JacksonXmlProperty(localName = "BusinessHeaderDocument")
    @NotNull(message = "Business header document cannot be null")
    private FSUBusinessHeaderDocument businessHeaderDocument;

    @Valid
    @JacksonXmlProperty(localName = "MasterConsignment")
    @NotNull(message = "Master Consignment cannot be null")
    private FSUMasterConsignment masterConsignment;

}
