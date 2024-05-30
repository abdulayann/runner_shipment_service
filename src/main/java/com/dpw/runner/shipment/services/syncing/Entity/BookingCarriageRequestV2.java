package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class BookingCarriageRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("CarriageType")
    private String CarriageType;
    @JsonProperty("CarriageMode")
    private String CarriageMode;
    @JsonProperty("PolId")
    private Integer PolId;
    @JsonProperty("PodId")
    private Integer PodId;
    @JsonProperty("Eta")
    private LocalDateTime Eta;
    @JsonProperty("Etd")
    private LocalDateTime Etd;
    @JsonProperty("Vessel")
    private String Vessel;
    @JsonProperty("VesselId")
    private Long VesselId;
    @JsonProperty("Voyage")
    private String Voyage;
    @JsonProperty("PortOfLoading")
    private String portOfLoading;
    @JsonProperty("PortOfDischarge")
    private String portOfDischarge;
}
