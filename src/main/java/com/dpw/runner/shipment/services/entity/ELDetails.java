package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;

import com.dpw.runner.shipment.services.entity.enums.MergeClass;
import lombok.*;
import lombok.experimental.Accessors;


@Entity
@Setter
@Getter
@Table(name = "el_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ELDetails {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    @ToString.Include
    private Long id;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "el_number")
    private String elNumber;

    @Column(name = "packages")
    private Long packages;

    @Column(name = "unit")
    private String unit;

    @Column(name = "weight")
    private Long weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Enumerated(EnumType.STRING)
    @Column(name = "merge_class")
    private MergeClass mergeClass;

    @Column(name = "merge_package")
    private Long mergePackage;

    @Column(name = "merge_package_unit")
    private String mergePackageUnit;

    @Column(name = "partition")
    private Boolean partition;

    @Column(name = "partition_seq_number")
    private Long partitionSeqNumber;

}
