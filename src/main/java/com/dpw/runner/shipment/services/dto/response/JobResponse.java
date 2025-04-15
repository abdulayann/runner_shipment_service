package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class JobResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private PartiesResponse buyerDetail;
    private PartiesResponse supplierDetail;
    private String orderNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime orderDate;
    private String confirmNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime confirmDate;
    private String invoiceNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime invoiceDate;
    private Long buyerId;
    private String orderStatus;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime followUpDate;
    private String description;
    private String currency;
    private String serviceMode;
    private String incoTerm;
    private String additionalTerms;
    private String transportMode;
    private String countryOfOrigin;
    private List<EventsResponse> eventsList;
}
