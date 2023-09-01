package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class BookingCarriageRequestV2 {
    private String CarriageType;
    private String CarriageMode;
    private Integer PolId;
    private Integer PodId;
    private DateTime? Eta;
    private DateTime? Etd;
    private String Vessel;
    private Int64 VesselId;
    private String Voyage;
}
