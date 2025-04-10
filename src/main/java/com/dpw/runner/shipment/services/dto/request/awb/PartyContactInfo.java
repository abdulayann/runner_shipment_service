package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@Builder
@ApiModel("Party Contact Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PartyContactInfo implements Serializable {

    private String contactPersonName;
    private String contactPersonTelephone;
    private String email;
    private String securityTextualInfo;
    private String authorisedEconomicOperator;
    private String tradeIdentification;
}
