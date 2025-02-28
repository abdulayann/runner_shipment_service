package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ArrivalDepartureDetailsModel implements IDocumentModel {
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime firstForeignPortArrivalDate;
    @JsonProperty("LastForeignPortDepartureDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime lastForeignPortDepartureDate;
}
