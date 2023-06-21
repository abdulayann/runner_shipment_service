package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "reference_numbers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ReferenceNumbers extends MultiTenancy {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    @ToString.Include
    private Long id;

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

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'shipment'")
    private List<ShipmentDetails> shipmentDetails;


}
