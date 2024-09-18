package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.entity.SpecifiedAddressLocation;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PartyAddress {

    @JsonProperty("PostcodeCode")
    @Size(max = 9, message = "Postal Structured Address post code can have max length {max}")
    private String postcodeCode;

    @JsonProperty("StreetName")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 70, message = "Postal Structured Address street name can have max length {max}")
    @NotNull(message = "Party streetName cannot be null")
    private String streetName;

    @JsonProperty("CityName")
    @StringModifier(maxLength = 17, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 17, message = "Postal Structured Address city name can have max length {max}")
    @NotNull(message = "Postal Structured Address city cannot be null")
    private String cityName;

    @JsonProperty("CountryID")
    @Size(max = 2, message = "Postal Structured Address country id can have max length {max}")
    @NotNull(message = "Postal Structured Address country id cannot be null")
    private String countryID;

    @JsonProperty("CountryName")
    @StringModifier(maxLength = 70, pattern = StringModifier.PatternType.ALPHA_NUMERIC)
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid postal structured address country name provided")
    @Size(max = 70, message = "Postal structured address country name can have max length {max}")
    private String countryName;

    @JsonProperty("CountrySubDivisionName")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid postal structured address country subdivision name provided")
    @Size(max = 9, message = "Postal structured address country subdivision name can have max length {max}")
    private String countrySubDivisionName;

    @JsonProperty("PostOfficeBox")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid postal structured address post office box provided")
    @Size(max = 35, message = "Postal structured address post office box can have max length {max}")
    private String postOfficeBox;

    @JsonProperty("CityId")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid postal structured address city id provided")
    @Size(max = 3, message = "Postal structured address city id can have max length {max}")
    private String cityId;

    @JsonProperty("CountrySubDivisionId")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid postal structured address country sub division id provided")
    @Size(max = 35, message = "Postal structured address country sub division id can have max length {max}")
    private String countrySubDivisionId;


    /** SpecifiedAddressLocation only applicable for Associated party */
    @Valid
    @JsonProperty("SpecifiedAddressLocation")
    private SpecifiedAddressLocation specifiedAddressLocation;
}
