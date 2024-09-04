package com.dpw.runner.shipment.services.dto.request.platform;

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
    private String vessel_name;
    private String destination_code;
    private String voyage_number;
    private String transport_mode;
    private String vessel_imo;
    private String vessel_mmsi;
    private String carrier_scac_code;
    private LocalDateTime eta;
    private LocalDateTime ets;
    private LocalDateTime ata;
    private LocalDateTime ats;
    private String airline_iata_code;
    private String airline_prefix;
    private String flight_number;
}
