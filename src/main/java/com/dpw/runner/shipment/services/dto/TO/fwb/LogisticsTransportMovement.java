package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LogisticsTransportMovement {

    @JsonProperty("StageCode")
    @NotNull(message = "Invalid Logistics Transport Movement stage code cannot be null")
    private String stageCode;

    @JsonProperty("ModeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Logistics Transport Movement mode code provided")
    @Size(max = 15, message = "Logistics Transport Movement mode code can have max length {max}")
    private String modeCode;

    @JsonProperty("Mode")
    @Size(max = 35, message = "Logistics Transport Movement mode can have max length {max}")
    private String mode;

    /** Reference to the unique conveyance ID of the means of transport (e.g. flight number, voyage number).
     * For Air mode, It consists of carrier code followed by flight number */

    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Logistics Transport Movement Id provided")
    @Size(max = 35, message = "Logistics Transport Movement Id can have max length {max}")
    // meaning: Means of Transport ID
    private String id;

    @JsonProperty("SequenceNumeric")
    @Min(value = 0, message = "Invalid Logistics Transport Movement sequence numeric must be greater than or equal to 0")
    @Max(value = 9999, message = "Invalid Logistics Transport Movement sequence numeric must be less than or equal to 9999")
    // meaning: Sequence of the transport, Contains the incremental number that identifies the sequence
    private Integer sequenceNumeric;

    // TODO: Conditional For Air only IATA Airline and Company Designators code should be used
    @JsonProperty("UsedLogisticsTransportMeansName")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Logistics transport movement logistics transport means name provided")
    @Size(max = 70, message = "Logistics transport movement logistics transport means name can have only max length {max}")
    // Name of the Mean of Transport (e.g. Carrier name)
    private String usedLogisticsTransportMeansName;

    @Valid
    @JsonProperty("ArrivalEvent")
    private ArrivalEventDto arrivalEvent;

    @Valid
    @JsonProperty("DepartureEvent")
    private DepartureEventDto departureEvent;

}
