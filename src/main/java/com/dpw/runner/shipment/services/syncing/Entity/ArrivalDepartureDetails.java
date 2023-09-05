package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ArrivalDepartureDetails {
    @JsonProperty("AcontainerYardId")
    public PartyRequestV2 AcontainerYardId;
    @JsonProperty("AfirstArrivalPortId")
    public PartyRequestV2 AfirstArrivalPortId;
    @JsonProperty("AlastForeignPortId")
    public PartyRequestV2 AlastForeignPortId;
    @JsonProperty("AfirstForeignPort")
    public String AfirstForeignPort;
    @JsonProperty("AlastForeignPort")
    public String AlastForeignPort;
    @JsonProperty("ACTOId")
    public PartyRequestV2 ACTOId;
    @JsonProperty("ACFSId")
    public PartyRequestV2 ACFSId;
    @JsonProperty("AfirstArrivalPortArrivalDate")
    public LocalDateTime AfirstArrivalPortArrivalDate;
    @JsonProperty("AlastForeignPortDepartureDate")
    public LocalDateTime AlastForeignPortDepartureDate;
    @JsonProperty("AtransportPortId")
    public PartyRequestV2 AtransportPortId;
    @JsonProperty("DcontainerYardId")
    public PartyRequestV2 DcontainerYardId;
    @JsonProperty("DtransportPortId")
    public PartyRequestV2 DtransportPortId;
    @JsonProperty("DCTOId")
    public PartyRequestV2 DCTOId;
    @JsonProperty("DCFSId")
    public PartyRequestV2 DCFSId;
    @JsonProperty("DfirstForeignPortArrivalDate")
    public LocalDateTime DfirstForeignPortArrivalDate;
    @JsonProperty("DfirstForeignPortId")
    public PartyRequestV2 DfirstForeignPortId;
    @JsonProperty("DfirstForeignPort")
    public String DfirstForeignPort;
    @JsonProperty("DlastForeignPortDepartureDate")
    public LocalDateTime DlastForeignPortDepartureDate;
    @JsonProperty("DlastForeignPortId")
    public PartyRequestV2 DlastForeignPortId;
    @JsonProperty("DlastForeignPort")
    public String DlastForeignPort;
}
