package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.math.BigDecimal;


@Entity
@Setter
@Getter
@Table(name = "ti_containers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE ti_containers SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TiContainers extends MultiTenancy {

    @Column(name = "ti_leg_id")
    private Long tiLegId;

    @NonNull
    @Column(name = "type")
    @MasterData(type = MasterDataType.CONTAINER_CATEGORY)
    private String type;

    @Column(name = "number")
    private String number;

    @Column(name = "description")
    @Size(max = 1024, message = "max size is 1024 for description")
    private String description;

    @Column(name = "no_of_packages")
    @Size(max = 5, message = "max size is 5 for noOfPackages")
    private String noOfPackages;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    private String grossWeightUnit;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "dangerous")
    private Boolean dangerous;

    @Column(name = "substance_name")
    @Size(max = 1024, message = "max size is 1024 for SubstanceName")
    private String substanceName;

    @Column(name = "un_number")
    @Size(max = 10, message = "max size is 10 for un_number")
    private String unNumber;

    @Column(name = "dg_class")
    @MasterData(type = MasterDataType.DG_CLASS)
    private String dgClass;

    @Column(name = "tunnel_restriction_code")
    @Size(max = 10, message = "max size is 10 for tunnel_restriction_code")
    private String tunnelRestrictionCode;
}
