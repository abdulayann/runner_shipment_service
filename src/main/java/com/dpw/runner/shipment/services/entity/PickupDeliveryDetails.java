package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

@Entity
@Setter
@Getter
@Table(name = "pickup_delivery_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class PickupDeliveryDetails extends MultiTenancy {

    @Column(name = "estimated_pickup_or_delivery")
    private LocalDateTime estimatedPickupOrDelivery;

    @Column(name = "required_by")
    private LocalDateTime requiredBy;

    @Column(name = "port_transport_advised")
    private LocalDateTime portTransportAdvised;

    @Column(name = "actual_pickup_or_delivery")
    private LocalDateTime actualPickupOrDelivery;

    @Column(name = "pickup_or_delivery")
    private LocalDateTime pickupOrDelivery;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'broker'")
    private List<Parties> partiesBrokerDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'agent'")
    private List<Parties> partiesAgentDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'transporter'")
    private List<Parties> partiesTransporterDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'destination'")
    private List<Parties> partiesDestinationDetailsList;

    @Column(name = "type")
    private String type;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
