package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChargeTypeResponse implements Serializable {

    private String id;
    private UUID guId;
    private String chargeCode;
    private String chargeCodeDescription;
    private Boolean isTaxable;
    private Boolean active;
    private Boolean isRcm;
    private String chargeGroup;
    private String arAccountCode;
    private String apAccountCode;
    private MeasurementBasis chargeMeasurementBasisCode;
    private String taxUniqueCode;
    private String tdsMasterId;
    private String hsnSacCode;
    private String taxWithRespectToCommodity;
    private BigDecimal taxRate;
    private String hsnSacCodeDescription;
}
