package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ArrivalDepartureDetails implements IRunnerRequest {
    @JsonProperty("AcontainerYardId")
    private PartyRequestV2 AcontainerYardId;
    @JsonProperty("AfirstArrivalPortId")
    private PartyRequestV2 AfirstArrivalPortId;
    @JsonProperty("AlastForeignPortId")
    private PartyRequestV2 AlastForeignPortId;
    @JsonProperty("AfirstForeignPort")
    private String AfirstForeignPort;
    @JsonProperty("AlastForeignPort")
    private String AlastForeignPort;
    @JsonProperty("ACTOId")
    private PartyRequestV2 ACTOId;
    @JsonProperty("ACFSId")
    private PartyRequestV2 ACFSId;
    @JsonProperty("AfirstArrivalPortArrivalDate")
    private LocalDateTime AfirstArrivalPortArrivalDate;
    @JsonProperty("AlastForeignPortDepartureDate")
    private LocalDateTime AlastForeignPortDepartureDate;
    @JsonProperty("AtransportPortId")
    private PartyRequestV2 AtransportPortId;
    @JsonProperty("DcontainerYardId")
    private PartyRequestV2 DcontainerYardId;
    @JsonProperty("DtransportPortId")
    private PartyRequestV2 DtransportPortId;
    @JsonProperty("DCTOId")
    private PartyRequestV2 DCTOId;
    @JsonProperty("DCFSId")
    private PartyRequestV2 DCFSId;
    @JsonProperty("DfirstForeignPortArrivalDate")
    private LocalDateTime DfirstForeignPortArrivalDate;
    @JsonProperty("DfirstForeignPortId")
    private PartyRequestV2 DfirstForeignPortId;
    @JsonProperty("DfirstForeignPort")
    private String DfirstForeignPort;
    @JsonProperty("DlastForeignPortDepartureDate")
    private LocalDateTime DlastForeignPortDepartureDate;
    @JsonProperty("DlastForeignPortId")
    private PartyRequestV2 DlastForeignPortId;
    @JsonProperty("DlastForeignPort")
    private String DlastForeignPort;
}
