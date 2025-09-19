package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class Charge implements Serializable {
    private String chargeType;
    private String chargeCode;
    private String chargeDescription;
    private String currency;
    private Double amount;
    private String paymentTerms;
    private String payableAt;
    private String chargeBasis;
    private String unitOfMeasure;
    private Double quantity;
    private String rateType;
    private Double rate;
}
