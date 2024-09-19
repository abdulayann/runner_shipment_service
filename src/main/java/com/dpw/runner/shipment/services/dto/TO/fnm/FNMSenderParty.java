package com.dpw.runner.shipment.services.dto.TO.fnm;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class FNMSenderParty {
    @JacksonXmlProperty(localName = "PrimaryID")
    @NotNull(message = "Sender Party primary id cannot be null")
    private FNMPrimaryID primaryID;
}
