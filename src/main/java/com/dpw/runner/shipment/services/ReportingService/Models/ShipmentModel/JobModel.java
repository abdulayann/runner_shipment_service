package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class JobModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("BuyerDetail")
    private PartiesModel buyerDetail;
    @JsonProperty("SupplierDetail")
    private PartiesModel supplierDetail;
    @JsonProperty("OrderNumber")
    private String orderNumber;
    @JsonProperty("OrderDate")
    private LocalDateTime orderDate;
    @JsonProperty("ConfirmNumber")
    private String confirmNumber;
    @JsonProperty("ConfirmDate")
    private LocalDateTime confirmDate;
    @JsonProperty("InvoiceNumber")
    private String invoiceNumber;
    @JsonProperty("InvoiceDate")
    private LocalDateTime invoiceDate;
    @JsonProperty("BuyerId")
    private Long buyerId;
    @JsonProperty("OrderStatus")
    private String orderStatus;
    @JsonProperty("FollowUpDate")
    private LocalDateTime followUpDate;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("Currency")
    private String currency;
    @JsonProperty("ServiceMode")
    private String serviceMode;
    @JsonProperty("IncoTerm")
    private String incoTerm;
    @JsonProperty("AdditionalTerms")
    private String additionalTerms;
    @JsonProperty("TransportMode")
    private String transportMode;
    @JsonProperty("CountryOfOrigin")
    private String countryOfOrigin;
    @JsonProperty("EventsList")
    private List<EventsModel> eventsList;
}
