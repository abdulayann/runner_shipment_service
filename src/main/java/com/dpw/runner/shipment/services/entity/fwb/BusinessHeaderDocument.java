package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BusinessHeaderDocument {

    // wayBill Number
    @JsonProperty("ID")
    @NotNull(message = "Waybill Number / Business Header Document Id should not be null")
    @Size(min = 1, max = 12, message = "Waybill Number / Business Header Document Id should be of {max}" )
    @Pattern(regexp = "^\\d{3}-\\d{8}$", message = "Invalid Waybill Number / Business Header Document Id provided")
    private String id;

    @JsonProperty("senderAssignedID")
    @Size(max = 35, message = "Business Header Document sender assigned id should be of {max}" )
    private String senderAssignedID;

    @JsonProperty("IncludedHeaderNote")
    private List<HeaderNote> includedHeaderNote;

    @JsonProperty("SignatoryConsignorAuthenticationName")
    @Size(max = 20, message = "Signatory consignor authentication mame should be of max length {max}")
    private String signatoryConsignorAuthenticationName;

    @Valid
    @JsonProperty("SignatoryCarrierAuthentication")
    @NotNull(message = "signatoryCarrierAuthentication can not be null")
    private SignatoryCarrierAuthentication signatoryCarrierAuthentication;

}
