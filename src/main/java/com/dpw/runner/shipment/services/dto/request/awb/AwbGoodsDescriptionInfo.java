package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Goods Description Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbGoodsDescriptionInfo {
    private Long entityId;
    private String entityType;
    private Integer piecesNo;
    private BigDecimal grossWt;
    private String grossWtUnit;
    private Integer rateClass;
    private Integer commodityItemNo;
    private BigDecimal chargeableWt;
    private BigDecimal rateCharge;
    private BigDecimal totalAmount;
    private Integer slaCCode;
    private String hsCode;
    private UUID guid;
    private List<AwbPackingInfo> awbPackingInfo;
    private Boolean isShipmentCreated;

}
