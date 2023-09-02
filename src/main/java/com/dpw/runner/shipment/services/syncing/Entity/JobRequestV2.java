package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class JobRequestV2 {
    private String AdditionalTerms;
    private PartyRequestV2 BuyerDetail;
    private LocalDateTime ConfirmDate;
    private String ConfirmNumber;
    private String CountryOfOrigin;
    private String Currency;
    private String Description;
    private List<EventsRequestV2> Events;
    private LocalDateTime FollowUpDate;
    private String IncoTerm;
    private LocalDateTime InvoiceDate;
    private String InvoiceNumber;
    private LocalDateTime OrderDate;
    private String OrderNumber;
    private String OrderStatus;
    private String ServiceMode;
    private PartyRequestV2 SupplierDetail;
    private String TransportMode;
}
