package com.dpw.runner.shipment.services.dto.TO.fzb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class LogisticsTransportMovementFZB {

    @JsonProperty("StageCode")
    @NotBlank(message = "StageCode is mandatory")
    @Size(max = 35, message = "StageCode must be at most 35 characters long")
    private String stageCode;

    @JsonProperty("ModeCode")
    @Size(max = 15, message = "ModeCode must be at most 15 characters long")
    private String modeCode;

    @JsonProperty("Mode")
    @Size(max = 35, message = "Mode must be at most 35 characters long")
    private String mode;

    @JsonProperty("ID")
    @Size(max = 35, message = "ID must be at most 35 characters long")
    private String ID;

    @JsonProperty("SequenceNumeric")
    //@NotBlank(message = "SequenceNumeric is mandatory")
    //@Pattern(regexp = "^\\d{4}$", message = "SequenceNumeric must be at most 4 characters long")
    private Integer sequenceNumeric;

////    @NotBlank(message = "Name is mandatory")
//    @Size(max = 70, message = "Name must be at most 70 characters long")
//    private String usedLogisticsTransportMeans;

    @JsonProperty("ArrivalEvent")
    @Valid
    private EventFZB arrivalEvent;

    @JsonProperty("DepartureEvent")
    @Valid
    private EventFZB departureEvent;
}
