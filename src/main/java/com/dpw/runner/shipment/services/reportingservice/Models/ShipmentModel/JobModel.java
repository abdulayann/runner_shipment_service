package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class JobModel implements IDocumentModel {
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime orderDate;
    @JsonProperty("ConfirmNumber")
    private String confirmNumber;
    @JsonProperty("ConfirmDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime confirmDate;
    @JsonProperty("InvoiceNumber")
    private String invoiceNumber;
    @JsonProperty("InvoiceDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime invoiceDate;
    @JsonProperty("BuyerId")
    private Long buyerId;
    @JsonProperty("OrderStatus")
    private String orderStatus;
    @JsonProperty("FollowUpDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
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
