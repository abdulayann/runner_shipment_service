package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "delivery")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DeliveryDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "estimated_delivery")
    private LocalDateTime estimatedDelivery;

    @Column(name = "required_by")
    private LocalDateTime requiredBy;

    @OneToMany(cascade=CascadeType.ALL, fetch = FetchType.EAGER,mappedBy="entityId")
    @Where(clause = "entity_type = 'Delivery'")
    private List<PartiesDetails> parties;

}
