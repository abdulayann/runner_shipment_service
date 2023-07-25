package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Parties;
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
    private LocalDateTime orderDate;
    private String confirmNumber;
    private LocalDateTime confirmDate;
    private String invoiceNumber;
    private LocalDateTime invoiceDate;
    private Long buyerId;
    private String orderStatus;
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
