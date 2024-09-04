package com.dpw.runner.shipment.services.dto.request.platform;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RouteLegRequest implements Serializable {
    private String order;
    private String origin_code;
    @JsonProperty(value = "vessel_name")
    private String vesselName;
    private String destination_code;
    @JsonProperty(value = "voyage_number")
    private String voyageNumber;
    private String transport_mode;
    @JsonProperty(value = "vessel_imo")
    private String vesselImo;
    @JsonProperty(value = "vessel_mmsi")
    private String vesselMmsi;
    @JsonProperty(value = "carrier_scac_code")
    private String carrierScacCode;
    private LocalDateTime eta;
    private LocalDateTime ets;
    private LocalDateTime ata;
    private LocalDateTime ats;
    @JsonProperty(value = "airline_iata_code")
    private String airlineIataCode;
    @JsonProperty(value = "airline_prefix")
    private String airlinePrefix;
    @JsonProperty(value = "flight_number")
    private String flightNumber;
}
