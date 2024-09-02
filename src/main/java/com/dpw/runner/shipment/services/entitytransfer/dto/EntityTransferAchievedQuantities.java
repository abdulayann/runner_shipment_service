package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.math.BigDecimal;
import java.util.Map;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class EntityTransferAchievedQuantities implements IEntityTranferBaseEntity {
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private BigDecimal consolidatedWeight;
    private String consolidatedWeightUnit;
    private BigDecimal consolidatedVolume;
    private String consolidatedVolumeUnit;
    private BigDecimal consolidationChargeQuantity;
    private String consolidationChargeQuantityUnit;
    private String weightUtilization;
    private String volumeUtilization;
    private Map<String, EntityTransferMasterLists> masterData;
}