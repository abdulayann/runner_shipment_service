package com.dpw.runner.shipment.services.dto.request.billing;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BillChargesFilterRequest extends BillingSearchRequest {

    private String moduleId;
    private String moduleTypeCode;
    private String chargeTypeId;
    private Boolean isARPosted;
    private Boolean isAPPosted;
    private String arInvoiceNumber;
    private String apInvoiceNumber;
    private Integer debtorId;
    private String debtorFullName;
    private Integer debtorAddressId;
    private String debtorAddress;
    private Integer creditorId;
    private String creditorFullName;
    private Integer creditorAddressId;
    private String creditorAddress;
    private String orderNumber;
    private String houseBill;
    private String masterBill;
    private String transportMode;
    private String shipmentType;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime etaDateFrom;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime etaDateTo;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime etdDateFrom;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime etdDateTo;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime createdDateFrom;
    @JsonFormat(pattern = Constants.DATE_TIME_FORMAT)
    private LocalDateTime createdDateTo;
    private List<String> billId;
    private List<String> ids;
    private String moduleNumber;
    private String currency;

}
