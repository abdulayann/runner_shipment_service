package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.PartiesDetails;
import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.UUID;

@Data
public class JobResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private List<PartiesDetails> partiesBuyerDetailsList;
    private List<PartiesDetails> partiesSupplierDetailsList;
    private String orderNumber;
    private Date orderDate;
    private String confirmNumber;
    private Date confirmDate;
    private String invoiceNumber;
    private Date invoiceDate;
    private Long buyerId;
    private String orderStatus;
    private Date followUpDate;
    private String description;
    private String currency;
    private String serviceMode;
    private String incoTerm;
    private String additionalTerms;
    private String transportMode;
    private String countryOfOrigin;
}
