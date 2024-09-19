package com.dpw.runner.shipment.services.dto.TO.fsu;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import lombok.*;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class FSUIncludedCustomsNote {

    @JacksonXmlProperty(localName ="Content")
    @Size(max = 35, message = "Included customs note content code can have max length {max}")
    @NotNull(message = "Included customs note content cannot be null")
    private String content;


    // TODO: Conditional, At least one of the three elements
    //  (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed

    @JacksonXmlProperty(localName ="ContentCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note content code provided")
    @Size(max = 2, message = "Included customs note content code can have max length {max}")
    private String contentCode;


    /**
     * TODO At least one of the three elements (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed
     */

    @JacksonXmlProperty(localName ="SubjectCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note subject code provided")
    @Size(max = 3, message = "Included customs note subject code can have max length {max}")
    private String subjectCode;


    /**
     * TODO At least one of the three elements (Country Code, Information Identifier or Customs, Security and Regulatory Control Information Identifier) must be completed
     */

    @JacksonXmlProperty(localName ="CountryId")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid included customs note subject provided")
    @Size(max = 2, message = "Included customs note subject can have max length {max}")
    private String countryId;
}
