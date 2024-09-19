package com.dpw.runner.shipment.services.dto.TO.fzb;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomsNoteFZB {

    @JsonProperty("ContentCode")
    @Size(max = 2, message = "Content code length must be 2 characters")
    @Pattern(regexp = "[A-Za-z]{2}", message = "Content code must contain only alphabets and be 2 characters long")
    private String contentCode;

    @JsonProperty("Content")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.TEXT)
    //@NotBlank(message = "Issue agent Number cannot be blank")
    @Size(max = 35, message = "Issue agent Number length must be less than or equal to 35 characters")
    //@Pattern(regexp = "^[a-zA-Z0-9.\\- /]*$", message = "CustomsNoteFZB content Pattern mismatch")
    private String content;

    @JsonProperty("SubjectCode")
    @Size(max = 3, message = "Subject code length must be 3 characters")
    @Pattern(regexp = "[A-Za-z]{3}", message = "Subject code must contain only alphabets and be 3 characters long")
    private String subjectCode;

    @JsonProperty("CountryId")
    @Size(max = 2, message = "Country ID length must be 2 characters")
    @Pattern(regexp = "[A-Za-z]{2}", message = "Country ID must contain only alphabets and be 2 characters long")
    private String countryID;
}
