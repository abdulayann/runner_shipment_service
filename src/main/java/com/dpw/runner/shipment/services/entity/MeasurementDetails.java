package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "measurment_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MeasurementDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "volumetric_weight")
    private BigDecimal volumetricWeight;

    @Column(name = "volumetric_weight_unit")
    private String volumetricWeightUnit;

    @Column(name = "chargable")
    private BigDecimal chargable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "no_of_packs")
    private Integer noOfPacks;

    @Column(name = "packs_unit")
    private String packsUnit;

    @Column(name = "inner_packs")
    private Integer innerPacks;

    @Column(name = "inner_pack_unit")
    private String innerPackUnit;
}
