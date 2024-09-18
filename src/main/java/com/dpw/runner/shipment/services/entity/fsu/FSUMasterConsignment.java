package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class FSUMasterConsignment {

    @JacksonXmlProperty(localName ="GrossWeightMeasure")
    private GrossWeightMeasure grossWeightMeasure;

    @JacksonXmlProperty(localName ="TotalGrossWeightMeasure")
    private TotalGrossWeightMeasure totalGrossWeightMeasure;

    @JacksonXmlProperty(localName ="PieceQuantity")
    @NotNull(message = "Master consignment total piece quantity cannot be null")
    private Integer pieceQuantity;

    @JacksonXmlProperty(localName ="TotalPieceQuantity")
    @NotNull(message = "Master consignment total piece quantity cannot be null")
    @Digits(integer = 4, fraction = 0, message = "Invalid master consignment total piece quantity provided")
    private Integer totalPieceQuantity;

    @JacksonXmlProperty(localName ="TransportSplitDescription")
    @NotNull(message = "Master consignment Transport split description cannot be null")
    @Size(max = 1, message = "Master consignment transport split description can have max length {max}")
    private String transportSplitDescription;

    @Valid
    @JacksonXmlProperty(localName ="TransportContractDocument")
    @NotNull(message = "Transport contract document cannot be null")
    private TransportContractDocument transportContractDocument;

    @Valid
    @JacksonXmlProperty(localName ="OriginLocation")
    @NotNull(message = "Master consignment origin location cannot be null")
    private FSULocationDto originLocation;

    @Valid
    @JacksonXmlProperty(localName ="FinalDestinationLocation")
    @NotNull(message = "Master consignment final Destination Location cannot be null")
    private FSULocationDto finalDestinationLocation;

    @Valid
    @JacksonXmlProperty(localName ="RoutingLocation")
    private FSURoutingLocationDto routingLocation;

    @Valid
    @JacksonXmlProperty(localName ="ReportedStatus")
    @JacksonXmlElementWrapper(useWrapping = false)
    @Size(min = 1, message = "At least one master consignment reported status should be provided")
    private List<ReportedStatus> reportedStatus;

}
