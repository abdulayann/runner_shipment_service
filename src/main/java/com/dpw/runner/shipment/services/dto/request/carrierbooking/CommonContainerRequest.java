package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonContainerRequest implements Serializable {
    private Long id;
    private UUID guid;
    private String containerCode;
    private Long count;
    private String goodsDescription;
    private String hsCode;
    private String commodityCode;
    private BigDecimal grossWeight;
    private BigDecimal volume;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private String grossWeightUnit;
    private String volumeUnit;
    private String containerNo;
    private Integer packs;
    private String packsUnit;
    private BigDecimal tareWeight;
    private String tareWeightUnit;
    private String sealNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
    private String shipperSealNumber;
    private String veterinarySealNumber;
    private String customsSealNumber;
    private String approvalSignature;
    private LocalDateTime approvalDate;
    private BigDecimal vgmWeight;
    private String vgmWeightUnit;
    private LocalDateTime weightDeterminationDateTime;
    private WeightDeterminationMethodType weightDeterminationMethod;
    private String weightDeterminationLocation;
    private String vgmStatus;
    private Long verifiedGrossMassId;
    private String commodityGroup;
    private String marksNums;
    private PartiesRequest weighingParty;
    private UUID containerRefGuid;
}
