package com.dpw.runner.shipment.services.dto.TO.fnm;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JacksonXmlRootElement(localName = "Response")
public class FNMPayload {

    @Valid
    @JacksonXmlProperty(localName = "MessageHeaderDocument")
    @NotNull(message = "messageHeaderDocument is required")
    private FNMMessageHeaderDocument messageHeaderDocument;

    @Valid
    @JacksonXmlProperty(localName = "BusinessHeaderDocument")
    @NotNull(message = "businessHeaderDocument is required")
    private FNMBusinessHeaderDocument businessHeaderDocument;

    @JacksonXmlProperty(localName = "ResponseStatus")
    private FNMResponseStatus responseStatus;

    @Size(max = 5000, message = "responseDetail exceeds the maximum size")
    @JacksonXmlProperty(localName = "ResponseDetail")
    private String responseDetail;
}
