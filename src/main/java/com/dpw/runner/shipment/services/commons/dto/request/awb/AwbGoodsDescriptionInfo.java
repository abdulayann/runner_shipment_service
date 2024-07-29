package com.dpw.runner.shipment.services.commons.dto.request.awb;

import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Goods Description Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbGoodsDescriptionInfo implements Serializable {
    private Long entityId;
    private String entityType;
    private Integer piecesNo;
    private BigDecimal grossWt;
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
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
    private Boolean disableFetchRates;
    private Boolean disableRates;
    private Boolean enableFetchRatesWarning;
}
