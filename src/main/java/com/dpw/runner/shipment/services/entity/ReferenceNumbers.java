package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "reference_numbers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ReferenceNumbers extends BaseEntity {

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "country_of_issue")
    private String countryOfIssue;

    @Column(name = "type")
    private String type;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "tenant_id")
    private String tenantId;

    @Column(name = "shipment_id")
    private String shipmentId;

}
