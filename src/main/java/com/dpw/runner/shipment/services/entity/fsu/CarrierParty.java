package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class CarrierParty {
    @JacksonXmlProperty(localName ="PrimaryID")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid carrier party id provided")
    @Size(max = 1, message = "Carrier party id can have only max length {max}")
    @NotNull(message = "CarrierParty primary id cannot be null")
    private String primaryId;
}
