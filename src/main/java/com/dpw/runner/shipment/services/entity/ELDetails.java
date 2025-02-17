package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.math.BigDecimal;


@Entity
@Setter
@Getter
@Builder
@Table(name = "el_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE el_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ELDetails extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "el_number")
    private String elNumber;

    @Column(name = "packages")
    private Long packages;

    @Column(name = "unit")
    private String unit;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Enumerated(EnumType.STRING)
    @Column(name = "merge_class")
    private MergeClass mergeClass;

    @Column(name = "merge_package")
    private Long mergePackages;

    @Column(name = "merge_package_unit")
    private String mergePackageUnit;

    @Column(name = "partition")
    private Boolean partition;

    @Column(name = "partition_seq_number")
    private Long partitionSeqNumber;
}
