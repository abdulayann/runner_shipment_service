package com.dpw.runner.shipment.services.dto.request.awb;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.io.Serializable;
import java.util.List;

@Data
@Builder
@Schema(description = "OCI Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OCIInfo implements Serializable {

    private PartyContactInfo shipper;
    private PartyContactInfo consignee;
    private PartyContactInfo notifyParty;
    private CustomerAccountInfo customerAccountInfo;
    private List<CustomerAccountInfo> customerAccountInfoList;
    private OtherIdentityInfo otherIdentityInfo;
}
