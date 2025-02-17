package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.OtherPartyType;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OtherPartyInfo implements IRunnerResponse {

    private OtherPartyType party;

    @Size(max = 35, message = "Additional ID must be up to 35 characters.")
    private String additionalId;

    @Size(max = 9, message = "State Name must be up to 9 characters.")
    private String stateName;

    @Size(max = 35, message = "Post Office Box must be up to 35 characters.")
    private String postOfficeBox;

    @Size(max = 3, message = "City ID must be up to 3 characters.")
    private String cityId;

    @Size(max = 70, message = "Contact Person Department must be up to 70 characters.")
    private String contactPersonDepartment;

    @Pattern(
            regexp = "^[A-Z0-9]{1,35}$",
            message = "FAX Number must be alphanumeric (A-Z, 0-9) and up to 35 characters."
    )
    private String faxNumber;

    @Size(max = 70, message = "Email ID must be up to 70 characters.")
    private String emailId;

    @Size(max = 35, message = "Telex Number must be up to 35 characters.")
    private String telexNumber;
}
