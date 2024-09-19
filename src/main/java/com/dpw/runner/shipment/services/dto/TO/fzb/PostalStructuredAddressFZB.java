package com.dpw.runner.shipment.services.dto.TO.fzb;
import com.dpw.runner.shipment.services.utils.annotation.StringModifier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class PostalStructuredAddressFZB {

    private String postcodeCode;

    @JsonProperty("StreetName")
    @StringModifier(maxLength = 35, pattern = StringModifier.PatternType.TEXT)
    @Size(max = 35, message = "Postal Structured Address street name can have max length {max}")
    @NotNull(message = "Party streetName(FZB) cannot be null")
    private String streetName;

    @JsonProperty("CityName")
    @StringModifier(maxLength = 17, pattern = StringModifier.PatternType.TEXT)
    @NotBlank(message = "party cityName is mandatory")
    private String cityName;

    @JsonProperty("CountryID")
    @NotBlank(message = "party Country code  is mandatory")
    private String countryID;

    private String countryName;

    private String countrySubDivisionName;

    private String postOfficeBox;

    private String cityID;

    private String countrySubDivisionID;

}
