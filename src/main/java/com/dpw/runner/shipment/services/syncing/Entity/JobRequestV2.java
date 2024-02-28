package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class JobRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("AdditionalTerms")
    private String AdditionalTerms;
    @JsonProperty("BuyerDetail")
    private PartyRequestV2 BuyerDetail;
    @JsonProperty("ConfirmDate")
    private LocalDateTime ConfirmDate;
    @JsonProperty("ConfirmNumber")
    private String ConfirmNumber;
    @JsonProperty("CountryOfOrigin")
    private String CountryOfOrigin;
    @JsonProperty("Currency")
    private String Currency;
    @JsonProperty("Description")
    private String Description;
    @JsonProperty("Events")
    private List<EventsRequestV2> Events;
    @JsonProperty("FollowUpDate")
    private LocalDateTime FollowUpDate;
    @JsonProperty("IncoTerm")
    private String IncoTerm;
    @JsonProperty("InvoiceDate")
    private LocalDateTime InvoiceDate;
    @JsonProperty("InvoiceNumber")
    private String InvoiceNumber;
    @JsonProperty("OrderDate")
    private LocalDateTime OrderDate;
    @JsonProperty("OrderNumber")
    private String OrderNumber;
    @JsonProperty("OrderStatus")
    private String OrderStatus;
    @JsonProperty("ServiceMode")
    private String ServiceMode;
    @JsonProperty("SupplierDetail")
    private PartyRequestV2 SupplierDetail;
    @JsonProperty("TransportMode")
    private String TransportMode;
}
