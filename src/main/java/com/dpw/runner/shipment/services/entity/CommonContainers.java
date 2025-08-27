package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

@Entity
@Table(name = "common_containers")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonContainers extends MultiTenancy {

    @Column(name = "container_code")
    @DedicatedMasterData(type = Constants.CONTAINER_TYPE_MASTER_DATA)
    private String containerCode;

    @Column(name = "count")
    private Integer count;

    @Column(name = "goods_description", columnDefinition = "TEXT")
    private String goodsDescription;

    @Column(name = "hs_code")
    private String hsCode;

    @Column(name = "commodity_code")
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private String commodityCode;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "volume_unit", length = 10)
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name = "container_no")
    private String containerNo;

    @Column(name = "packs")
    private Integer packs;

    @Column(name = "packs_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    @Column(name = "tare_weight")
    private BigDecimal tareWeight;

    @Column(name = "tare_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String tareWeightUnit;

    @Column(name = "seal_number")
    private String sealNumber;

    @Column(name = "carrier_booking_id")
    private Long carrierBookingId;

    @Column(name = "shipping_instruction_id")
    private Long shippingInstructionId;
}
