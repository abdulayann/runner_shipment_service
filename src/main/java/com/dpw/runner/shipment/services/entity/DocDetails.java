package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;
import lombok.*;

import javax.persistence.*;

@Entity
@Getter
@Setter
@Table(name = "doc_details")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DocDetails extends MultiTenancy {

    @Column(name = "version_number")
    private String versionNumber;

    @Column(name = "type")
    @Enumerated(value = EnumType.STRING)
    private DocDetailsTypes type;

    @Column(name = "entity_id")
    private Long entityId;

}
