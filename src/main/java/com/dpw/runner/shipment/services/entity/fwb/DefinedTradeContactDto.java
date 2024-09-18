package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DefinedTradeContactDto {

    @JsonProperty("PersonName")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid defined trade contact person name provided")
    @Size(max = 70, message = "Defined trade contact person name can have max length {max}")
    private String personName;

    @JsonProperty("DepartmentName")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid defined trade contact department name provided")
    @Size(max = 70, message = "Defined trade contact department name can have max length {max}")
    private String departmentName;

    @JsonProperty("DirectTelephoneCommunicationNumber")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Direct Telephone Communication Number provided")
    @Size(max = 25, message = "Direct Telephone Communication Number can have max length {max}")
    private String directTelephoneCommunicationNumber;

    @JsonProperty("FaxCommunicationNumber")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Fax Communication Number provided")
    @Size(max = 25, message = "Fax Communication Number can have max length {max}")
    private String faxCommunicationNumber;

    @JsonProperty("URIEmailCommunicationId")
    @Size(max = 70, message = "Uri Email Communication Id can have max length {max}")
    private String uriEmailCommunicationId;

    @JsonProperty("TelexCommunicationNumber")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Telex Communication Number provided")
    @Size(max = 35, message = "Telex Communication Number can have max length (max)")
    private String telexCommunicationNumber;

}
