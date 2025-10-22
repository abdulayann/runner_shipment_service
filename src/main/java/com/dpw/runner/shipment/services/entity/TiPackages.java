package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

@Entity
@Setter
@Getter
@Table(name = "ti_packages")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE ti_packages SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TiPackages extends MultiTenancy {

    @Column(name = "ti_leg_id")
    private Long tiLegId;

    @Column(name = "no_of_packages")
    @Size(max=5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;

    @Column(name = "package_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packageType;

    @Column(name = "description")
    @Size(max=1024, message = "max size is 1024 for description")
    private String description;

    @Column(name = "length")
    private BigDecimal length;

    @Column(name = "length_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String lengthUnit;

    @Column(name = "width")
    private BigDecimal width;

    @Column(name = "width_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String widthUnit;

    @Column(name = "height")
    private BigDecimal height;

    @Column(name = "height_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String heightUnit;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name = "dangerous")
    private Boolean dangerous;

    @Column(name = "substance_name")
    @Size(max=1024, message = "max size is 1024 for substance_name")
    private String substanceName;

    @Column(name = "un_number")
    @Size(max=10, message = "max size is 10 for un_Number")
    private String unNumber;

    @Column(name = "hazard_label")
    @MasterData(type = MasterDataType.DG_CLASS)
    private String hazardLabel;

    @Column(name = "tunnel_restriction_code")
    @Size(max=10, message = "max size is 10 for tunnel_restriction_code")
    private String tunnelRestrictionCode;

    @Column(name = "proper_shipping_name")
    @Size(max=63, message = "max size is 63 for proper shipping name")
    private String properShippingName;

    @Column(name = "packing_group")
    @Size(max=31, message = "max size is 31 for packing group")
    @MasterData(type = MasterDataType.PACKING_GROUP)
    private String packingGroup;

    @Column(name = "minimum_flash_point")
    private BigDecimal minimumFlashPoint;

    @Column(name = "minimum_flash_point_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    @Size(max = 3, message = "max size is 3 for minimum flash point unit")
    private String minimumFlashPointUnit;

    @Column(name = "marine_pollutant")
    private Boolean marinePollutant = false;

    @Column(name = "dg_class_description")
    @Size(max=255, message = "max size is 255 for dg class description")
    private String dgClassDescription;
}
