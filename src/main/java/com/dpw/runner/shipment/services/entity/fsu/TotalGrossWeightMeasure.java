package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlText;
import lombok.*;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class TotalGrossWeightMeasure {

    @JacksonXmlText
    @DecimalMin(value = "0.0", message = "Master Consignment Total gross weight measure must be greater than or equal to 0.0")
    @DecimalMax(value = "9999999", message = "Master Consignment Total gross weight measure must be less than or equal to 9999999")
    @NotNull(message = "Master Consignment Total gross weight measure cannot be null")
    private String value;

    @JacksonXmlProperty(isAttribute = true, localName ="unitCode")
    @NotNull(message = "Master Consignment Total gross weight measure unit cannot be null")
    private String unitCode;
}
