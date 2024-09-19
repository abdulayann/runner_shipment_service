package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeliveryParty {

    @JacksonXmlProperty(localName = "Name")
    @Size(max = 70, message = "Delivery Party name can have only max length {max}")
    private String name;
}
