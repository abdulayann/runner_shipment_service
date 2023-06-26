package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import lombok.Getter;
import lombok.ToString;

import java.util.Date;
import java.util.List;

@Getter
@ToString
public class JobRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private List<Parties> partiesBuyerDetailsList;
    private List<Parties> partiesSupplierDetailsList;
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
