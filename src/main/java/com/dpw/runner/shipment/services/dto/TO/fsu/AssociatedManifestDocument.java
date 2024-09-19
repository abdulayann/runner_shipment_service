package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class AssociatedManifestDocument {

    @JacksonXmlProperty(localName = "ID")
    @DecimalMin(value = "0.0", message = "Status consignment density group code must be greater than or equal to 0.0")
    @DecimalMax(value = "999999", message = "Status consignment density group code must be less than or equal to 999999")
    @NotNull(message = "Associated manifest document id cannot be null")
    private Double id;
}
