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
public class AssociatedConsignmentCustomsProcedure {

    @JacksonXmlProperty(localName ="GoodsStatusCode")
    @Size(max = 2, message = "Associated consignment customs procedure code can have max length {max}")
    private String goodsStatusCode;
}
