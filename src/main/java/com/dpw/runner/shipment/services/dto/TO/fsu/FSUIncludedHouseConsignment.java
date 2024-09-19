package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUIncludedHouseConsignment {

    @JacksonXmlProperty(localName ="GrossWeightMeasure")
    @DecimalMin(value = "0.0", message = "Included house consignment gross weight measure should be >= 0.0")
    @DecimalMax(value = "9999999", message = "Included house consignment gross weight measure should be <= 9999999")
    private Double grossWeightMeasure;

    @JacksonXmlProperty(localName ="GrossWeightMeasureUnit")
    private String grossWeightMeasureUnit;

    @JacksonXmlProperty(localName ="TotalGrossWeightMeasure")
    @DecimalMin(value = "0.0", message = "Included house consignment total gross weight measure should be >= 0.0")
    @DecimalMax(value = "9999999", message = "Included house consignment total gross weight measure should be <= 9999999")
    private Double totalGrossWeightMeasure;

    @JacksonXmlProperty(localName ="TotalGrossWeightMeasureUnit")
    private String totalGrossWeightMeasureUnit;

    @JacksonXmlProperty(localName ="PieceQuantity")
    @Digits(integer = 4, fraction = 0, message = "Invalid included house consignment piece quantity provided")
    @NotNull(message = "Included house consignment piece quantity cannot be null")
    private Integer pieceQuantity;

    //todo Must be completed if there is a Part Consignment
    @JacksonXmlProperty(localName ="TotalPieceQuantity")
    @Digits(integer = 4, fraction = 0, message = "Invalid included house consignment total piece quantity provided")
    @NotNull(message = "Included house consignment total piece quantity cannot be null")
    private Integer totalPieceQuantity;

    @JacksonXmlProperty(localName ="TransportSplitDescription")
    @NotNull(message = "Included house consignment transport split description cannot be null")
    @Size(max = 1, message = "Included house consignment transport split description can have max length {max}")
    private String transportSplitDescription;

    @Valid
    @JacksonXmlProperty(localName ="TransportContractDocument")
    @NotNull(message = "Transport contract document cannot be null")
    private TransportContractDocument transportContractDocument;
}
