package com.dpw.runner.shipment.services.entity.fnm;

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
public class FNMRecipientParty {
    @JacksonXmlProperty(localName = "PrimaryID")
    @NotNull(message = "Recipient Party primary id cannot be null")
    private FNMPrimaryID primaryID;
}
