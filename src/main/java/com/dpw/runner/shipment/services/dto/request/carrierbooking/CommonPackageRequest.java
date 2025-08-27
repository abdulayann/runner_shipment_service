package com.dpw.runner.shipment.services.dto.request.carrierbooking;


import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonPackageRequest {

    private String containerNo;
    private Integer packs;

    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    private String hsCode;
    private String commodityDescription;
    private String goodsDescription;

    private BigDecimal grossWeight;

    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    private BigDecimal volume;

    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    private Long shippingInstructionId;
}
