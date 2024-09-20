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
@Table(name = "mawb_hawb_link")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class MawbHawbLink extends MultiTenancy {
    @Column(name = "mawb_id")
    public Long mawbId;

    @Column(name = "hawb_id")
    public Long hawbId;
}
