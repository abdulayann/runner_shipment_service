package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AssociatedUnitLoadTransportEquipment {
    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated unit load transport equipment id provided")
    @Size(max = 5, message = "Associated unit load transport equipment id can have max length {max}")
    @NotNull(message = "Associated unit load transport equipment id cannot be null")
    private String id;

    @JsonProperty("TareWeightMeasure")
    @DecimalMin(value = "0.0", message = "Associated unit load transport equipment tare weight measure must be greater than or equal to 0.0")
    @DecimalMax(value = "9999999", message = "Associated unit load transport equipment tare weight measure must be greater than or equal to 9999999")
    private Double tareWeightMeasure;

    @JsonProperty("TareWeightMeasureUnit")
    private String tareWeightMeasureUnit;

    @JsonProperty("LoadedPackageQuantity")
    @DecimalMin(value = "0.0", message = "Associated unit load transport equipment loaded package quantity must be greater than or equal to 0.0")
    @DecimalMax(value = "99999", message = "Associated unit load transport equipment loaded package quantity must be greater than or equal to 99999")
    private Double loadedPackageQuantity;

    @JsonProperty("CharacteristicCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated unit load transport equipment characteristic code provided")
    @Size(max = 3, message = "Associated unit load transport equipment characteristic code can have max length {max}")
    @NotNull(message = "Associated unit load transport equipment characteristic code cannot be null")
    private String characteristicCode;

    @JsonProperty("OperatingPartyID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid associated unit load transport equipment operating party id provided")
    @Size(max = 2, message = "Associated unit load transport equipment operating party id can have max length {max}")
    @NotNull(message = "Associated unit load transport equipment operating party id cannot be null")
    private String operatingPartyId;

    @JsonProperty("OperatingPartySchemeAgentId")
    private String operatingPartySchemeAgentId = "1";
}
