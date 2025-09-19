package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonContainerResponse implements IRunnerResponse {

    private Long id;   // from MultiTenancy / BaseEntity
    private UUID guid;
    private String containerCode;
    private Integer count;
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
    private String shipperSealNumber;
    private String veterinarySealNumber;
    private String customsSealNumber;
    private String approvalSignature;
    private LocalDateTime approvalDate;
    private BigDecimal vgmWeight;
    private String vgmWeightUnit;
    private WeightDeterminationMethodType weightDeterminationMethod;
    private String weightDeterminationLocation;
    private String vgmStatus;
    private Long carrierBookingId;
    private Long shippingInstructionId;
    private Long verifiedGrossMassId;
    private String commodityGroup;
    private String marksNums;
    private Parties weighingParty;
}
