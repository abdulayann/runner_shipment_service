package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.Size;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSULogisticsTransportMovement {

    @JacksonXmlProperty(localName ="ID")
    @Size(max = 35, message = "Logistics Transport Movement Id can have max length {max}")
    private String id;

    @JacksonXmlProperty(localName ="UsedLogisticsTransportMeans")
    private UsedLogisticsTransportMeans usedLogisticsTransportMeans;

    @JacksonXmlProperty(localName ="ScheduledArrivalEvent")
    private ScheduledArrivalEvent scheduledArrivalEvent;

    @Valid
    @JacksonXmlProperty(localName ="ArrivalEvent")
    private FSUArrivalEventDto arrivalEvent;

    @Valid
    @JacksonXmlProperty(localName ="DepartureEvent")
    private FSUDepartureEventDto departureEvent;

    @JacksonXmlProperty(localName ="CarrierParty")
    private CarrierParty carrierParty;

    @Valid
    @JacksonXmlProperty(localName ="SpecifiedLocation")
    @JacksonXmlElementWrapper(useWrapping = false)
    private List<FSUSpecifiedLocation> specifiedLocations = new ArrayList<>();

    @Valid
    @JacksonXmlProperty(localName ="SpecifiedEvent")
    private FSUSpecifiedEventDto specifiedEvent;

}
