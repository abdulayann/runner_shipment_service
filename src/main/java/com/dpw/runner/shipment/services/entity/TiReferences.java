package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;

@Entity
@Setter
@Getter
@Table(name = "ti_reference")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class TiReferences extends MultiTenancy {

    @Column(name = "type")
    @MasterData(type = MasterDataType.REFERENCE_NUMBER_TYPE)
    private String type;

    @Column(name = "reference")
    @Size(max=30, message = "max size is 30 for reference")
    private String reference;
}
