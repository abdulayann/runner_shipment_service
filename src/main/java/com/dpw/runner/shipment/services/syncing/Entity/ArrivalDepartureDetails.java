package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ArrivalDepartureDetails {
    private PartyRequestV2 AcontainerYardId;
    private PartyRequestV2 AfirstArrivalPortId;
    private PartyRequestV2 AlastForeignPortId;
    private String AfirstForeignPort;
    private String AlastForeignPort;
    private PartyRequestV2 ACTOId;
    private PartyRequestV2 ACFSId;
    private LocalDateTime AfirstArrivalPortArrivalDate;
    private LocalDateTime AlastForeignPortDepartureDate;
    private PartyRequestV2 AtransportPortId;
    private PartyRequestV2 DcontainerYardId;
    private PartyRequestV2 DtransportPortId;
    private PartyRequestV2 DCTOId;
    private PartyRequestV2 DCFSId;
    private LocalDateTime DfirstForeignPortArrivalDate;
    private PartyRequestV2 DfirstForeignPortId;
    private String DfirstForeignPort;
    private LocalDateTime DlastForeignPortDepartureDate;
    private PartyRequestV2 DlastForeignPortId;
    private String DlastForeignPort;
}
