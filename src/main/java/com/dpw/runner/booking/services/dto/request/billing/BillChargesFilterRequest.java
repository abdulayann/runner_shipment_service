package com.dpw.runner.booking.services.dto.request.billing;

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
    private LocalDateTime etaDateFrom;
    private LocalDateTime etaDateTo;
    private LocalDateTime etdDateFrom;
    private LocalDateTime etdDateTo;
    private LocalDateTime createdDateFrom;
    private LocalDateTime createdDateTo;
    private List<String> billId;
    private List<String> ids;
    private String moduleNumber;
    private String currency;

}
