package com.dpw.runner.shipment.services.dto.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BillChargesRequest implements Serializable {
    private String id;
    @NotEmpty(message = "BillId can not be empty")
    private String billId;
    @NotEmpty(message = "ChargeType can not be empty")
    private String chargeTypeId;
    private String details;
    //to display bill charges in sorted order based on it
    private Integer sequenceNumber;
    private String paymentTypeCode;
    private Boolean isFromConsolidation = false;
    private List<String> containerGuids;
    private Boolean isArPosted;
    private Boolean isApPosted;
    private String ServiceType;
    @Valid
    private BillChargeCostDetailsRequest billChargeCostDetails;
    @Valid
    private BillChargeRevenueDetailsRequest billChargeRevenueDetails;
    private List<String> ignoreValidations;
    private List<String> autoCalculate;
}