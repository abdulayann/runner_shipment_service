package com.dpw.runner.shipment.services.commons.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;


@Entity
@Setter
@Getter
@Table(name = "dg_mapping")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class DGMapping extends MultiTenancy {

    @Column(name = "class_name")
    private String className;

    @Column(name = "dg_class")
    private String dgClass;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "packing_id")
    private Long packingId;

    @Column(name = "dg_substance_id")
    private Long dgSubstanceId;

}
