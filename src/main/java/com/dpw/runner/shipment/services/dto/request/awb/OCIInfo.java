package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@Builder
@ApiModel("AWB OCI Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OCIInfo implements Serializable {

    private PartyContactInfo shipper;
    private PartyContactInfo consignee;
    private PartyContactInfo notifyParty;
    private CustomerAccountInfo customerAccountInfo;
    private OtherIdentityInfo otherIdentityInfo;
}
