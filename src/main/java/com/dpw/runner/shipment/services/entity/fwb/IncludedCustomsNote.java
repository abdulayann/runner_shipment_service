package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IncludedCustomsNote {

    /** Supplementary information identifying a party or a location related to Customs reporting requirements */

    @JsonProperty("Content")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 35, message = "Included customs note content code can have max length {max}")
    //@NotNull(message = "Included customs note content cannot be null")
    private String content;

    /** Coded indicator qualifying Customs related information:
     * Item Number "I",
     * Exemption Legend "L",
     * System Downtime Reference "S",
     * Unique Consignment Reference Number "U",
     * Movement Reference Number "M"
     * */

    // TODO: Conditional, At least one of the three elements
    //  (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed

    @JsonProperty("ContentCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note content code provided")
    @Size(max = 2, message = "Included customs note content code can have max length {max}")
    private String contentCode;


    /** TODO At least one of the three elements (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed */
    /** Code identifying a piece of information/entity e.g. "IMP" for import, "EXP" for export, "AGT" for Agent, "ISS" for The Regulated Agent Issuing the Security Status for a Consignment etc. */

    @JsonProperty("SubjectCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note subject code provided")
    @Size(max = 3, message = "Included customs note subject code can have max length {max}")
    private String subjectCode;


    /** TODO At least one of the three elements (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed */
    /** Coded representation of a country approved by ISO */

    @JsonProperty("CountryId")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note subject provided")
    @Size(max = 2, message = "Included customs note subject can have max length {max}")
    private String countryId;
}
