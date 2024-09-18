package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TransportEquipment {
    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Transport Equipment Id provided")
    @Size(max = 70, message = "Transport Equipment Id can have max length {max}")
    // Contains identification number (e.g. vehicle registration number)
    private String id;

    // TODO: Conditional: For Maritime, the Type field would include Type & Size
    @JsonProperty("CharacteristicCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid transport equipment characteristic code provided")
    @Size(max = 35, message = "Transport equipment characteristic code can have max length {max}")
    // Contains the type of the Container or Vehicle
    private String characteristicCode;

    @JsonProperty("Characteristic")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid transport equipment characteristic  provided")
    @Size(max = 35, message = "Transport equipment characteristic can have max length {max}")
    // Describes the size of the Container or Vehicle. Free Text
    private String characteristic;

    @JsonProperty("AffixedLogisticsSealId")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid transport equipment affixed logistics seal id  provided")
    @Size(max = 35, message = "Transport equipment affixed logistics seal id can have max length {max}")
    // Contains the seal number affixed
    private String affixedLogisticsSealId;
}
