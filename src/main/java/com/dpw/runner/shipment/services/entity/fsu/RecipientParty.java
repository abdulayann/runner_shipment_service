package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecipientParty {
    @JacksonXmlProperty(localName = "PrimaryID")
    @NotNull(message = "Recipient Party primary id cannot be null")
    private PrimaryID primaryID;
}
