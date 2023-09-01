package com.dpw.runner.shipment.services.syncing.Entity;

import java.time.LocalDateTime;

public class ArrivalDepartureDetails {
    public PartyRequestV2 AcontainerYardId;
    public PartyRequestV2 AfirstArrivalPortId;
    public PartyRequestV2 AlastForeignPortId;
    public String AfirstForeignPort;
    public String AlastForeignPort;
    public PartyRequestV2 ACTOId;
    public PartyRequestV2 ACFSId;
    public LocalDateTime AfirstArrivalPortArrivalDate;
    public LocalDateTime AlastForeignPortDepartureDate;
    public PartyRequestV2 AtransportPortId;
    public PartyRequestV2 DcontainerYardId;
    public PartyRequestV2 DtransportPortId;
    public PartyRequestV2 DCTOId;
    public PartyRequestV2 DCFSId;
    public LocalDateTime DfirstForeignPortArrivalDate;
    public PartyRequestV2 DfirstForeignPortId;
    public String DfirstForeignPort;
    public LocalDateTime DlastForeignPortDepartureDate;
    public PartyRequestV2 DlastForeignPortId;
    public String DlastForeignPort;
}
