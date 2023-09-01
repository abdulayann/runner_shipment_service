package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class BookingCarriageRequestV2 {
    private String CarriageType;
    private String CarriageMode;
    private Integer PolId;
    private Integer PodId;
    private LocalDateTime Eta;
    private LocalDateTime Etd;
    private String Vessel;
    private Long VesselId;
    private String Voyage;
}
