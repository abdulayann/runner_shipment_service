package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;


@Entity
@Setter
@Getter
@Table(name = "hbl_release_type_mapping")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class HblReleaseTypeMapping extends MultiTenancy {

    @Column(name = "hbl_id")
    private Long hblId;

    @Column(name = "release_type")
    private String releaseType;

    @Column(name = "copies_printed")
    private Integer copiesPrinted;
}

