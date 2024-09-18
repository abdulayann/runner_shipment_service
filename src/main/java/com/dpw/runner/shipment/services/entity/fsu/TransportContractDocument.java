package com.dpw.runner.shipment.services.entity.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;


/**
 * Note:
 * TransportContractDocument present at two places,
 * - IncludedHouseConsignment
 * - MasterConsignment
 *
 * */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class TransportContractDocument {

    @JacksonXmlProperty(localName ="ID")
    @Size(max = 12, message = "Transport contract document id can have max length {max}")
    @NotNull(message = "Transport contract document id cannot be null")
    private String id;

    @JacksonXmlProperty(localName ="Name")
    @Size(max = 70, message = "Transport contract document name can have max length {max}")
    private String name;

    // 740 for Air Waybill, 703 for House Waybill
    @JacksonXmlProperty(localName ="TypeCode")
    @Digits(integer = 3, fraction = 0, message = "Invalid Transport contract document type code provided")
    @NotNull(message = "Transport contract document type code cannot be null")
    private Integer typeCode;
}
