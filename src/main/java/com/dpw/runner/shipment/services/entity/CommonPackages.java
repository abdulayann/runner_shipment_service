package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
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
@Table(name = "common_packages")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CommonPackages extends MultiTenancy {

    @Column(name = "container_no")
    private String containerNo;

    @Column(name = "packs")
    private Integer packs;

    @Column(name = "packs_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    @Column(name = "hs_code", length = 20)
    private String hsCode;

    @Column(name = "commodity_description", columnDefinition = "TEXT")
    private String commodityDescription;

    @Column(name = "goods_description", columnDefinition = "TEXT")
    private String goodsDescription;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name ="shipping_instruction_id")
    private Long shippingInstructionId;
}
