package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ApplicableLogisticsServiceCharge {

    @JacksonXmlProperty(localName = "ServiceTypeCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Applicable logistics service charge service type code  provided")
    @Size(max = 1, message = "Applicable logistics service charge service type code can have max length {max}")
    private String serviceTypeCode;
}
