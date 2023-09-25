package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class BookingCarriageModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("PortOfLoading")
    private String portOfLoading;
    @JsonProperty("PortOfDischarge")
    private String portOfDischarge;
    @JsonProperty("Eta")
    private LocalDateTime eta;
    @JsonProperty("Etd")
    private LocalDateTime etd;
    @JsonProperty("Vessel")
    private String vessel;
    @JsonProperty("Voyage")
    private String voyage;
    @JsonProperty("CarriageType")
    private String carriageType;
    @JsonProperty("CarriageMode")
    private String carriageMode;
}
