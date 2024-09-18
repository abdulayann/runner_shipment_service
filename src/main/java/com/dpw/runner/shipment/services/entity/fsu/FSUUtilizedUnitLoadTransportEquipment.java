package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUUtilizedUnitLoadTransportEquipment {
    @JacksonXmlProperty(localName ="ID")
    @Size(max = 5, message = "Utilized unit load transport equipment id can have max length {max}")
    @NotNull(message = "Utilized Unit load transport equipment id cannot be null")
    private String id;

    @JacksonXmlProperty(localName ="CharacteristicCode")
    @Size(max = 3, message = "Utilized unit load transport equipment characteristic code can have max length {max}")
    @NotNull(message = "Utilized unit load transport equipment characteristic code cannot be null")
    private String characteristicCode;

    @JacksonXmlProperty(localName ="OperationalStatusCode")
    @Size(max = 1, message = "Utilized unit load transport equipment operational status code can have max length {max}")
    @NotNull(message = "Utilized unit load transport equipment operational status code cannot be null")
    private String operationalStatusCode;

    @JacksonXmlProperty(localName ="OperatingPartyID")
    @Size(max = 2, message = "Utilized unit load transport equipment operating party id can have max length {max}")
    private String operatingPartyId;

    @JacksonXmlProperty(localName ="OperatingPartySchemeAgentId")
    private String operatingPartySchemeAgentId = "1";
}
