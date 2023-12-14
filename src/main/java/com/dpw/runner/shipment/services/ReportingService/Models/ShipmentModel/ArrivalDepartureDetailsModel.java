package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ArrivalDepartureDetailsModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("ContainerYardId")
    private PartiesModel containerYardId;
    @JsonProperty("TransportPortId")
    private PartiesModel transportPortId;
    @JsonProperty("CTOId")
    private PartiesModel CTOId;
    @JsonProperty("CFSId")
    private PartiesModel CFSId;
    @JsonProperty("FirstForeignPortId")
    private PartiesModel firstForeignPortId;
    @JsonProperty("LastForeignPortId")
    private PartiesModel lastForeignPortId;
    @JsonProperty("FirstForeignPort")
    private String firstForeignPort;
    @JsonProperty("LastForeignPort")
    private String lastForeignPort;
    @JsonProperty("Type")
    private String type;
    @JsonProperty("FirstForeignPortArrivalDate")
    private LocalDateTime firstForeignPortArrivalDate;
    @JsonProperty("LastForeignPortDepartureDate")
    private LocalDateTime lastForeignPortDepartureDate;
}
