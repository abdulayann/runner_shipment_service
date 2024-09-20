package com.dpw.runner.shipment.services.dto.GeneralAPIRequests;

import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
public class VolumeWeightChargeable {
    BigDecimal chargeable;
    String chargeableUnit;
    BigDecimal volumeWeight;
    String volumeWeightUnit;
}
