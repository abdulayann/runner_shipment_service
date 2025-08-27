package com.dpw.runner.shipment.services.dto.request.carrierbooking;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
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
public class CommonContainerRequest {

    @DedicatedMasterData(type = Constants.CONTAINER_TYPE_MASTER_DATA)
    private String containerCode;

    private Integer count;
    private String goodsDescription;
    private String hsCode;

    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private String commodityCode;

    private BigDecimal grossWeight;
    private BigDecimal volume;
    private BigDecimal netWeight;

    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    private String containerNo;
    private Integer packs;

    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    private BigDecimal tareWeight;

    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String tareWeightUnit;

    private String sealNumber;
    private Long carrierBookingId;
    private Long shippingInstructionId;
}
