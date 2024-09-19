package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IDdto {
    @JsonProperty("schemeID")
    private String schemeID;

    @JsonProperty("schemeName")
    private String schemeName;

    @JsonProperty("schemeAgencyIDSpecified")
    private Boolean schemeAgencyIDSpecified;

    @JsonProperty("schemeAgencyName")
    private String schemeAgencyName;

    @JsonProperty("schemeVersionID")
    private String schemeVersionID;

    @JsonProperty("schemeDataURI")
    private String schemeDataURI;

    @JsonProperty("schemeURI")
    private String schemeURI;

    @JsonProperty("value")
    @Size(max = 70, message = "Sender/Recipient party id can have max length 70")
    private String value;

}


