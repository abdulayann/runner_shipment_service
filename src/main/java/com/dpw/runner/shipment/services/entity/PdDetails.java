package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

@Entity
@Setter
@Getter
@Table(name = "pd_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class PdDetails extends MultiTenancy {

    @Column(name = "estimated_pickup")
    private LocalDateTime estimatedPickup;

    @Column(name = "required_by")
    private LocalDateTime requiredBy;

    @Column(name = "port_transport_advised")
    private LocalDateTime portTransportAdvised;

    @Column(name = "actual_pickup")
    private LocalDateTime actualPickup;

    @Column(name = "pickup")
    private LocalDateTime pickup;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'broker'")
    private List<PartiesDetails> partiesBrokerDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'agent'")
    private List<PartiesDetails> partiesAgentDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'transporter'")
    private List<PartiesDetails> partiesTransporterDetailsList;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'jobs' AND party_type = 'destination'")
    private List<PartiesDetails> partiesDestinationDetailsList;

    @Column(name = "type")
    private String type;

    @Column(name = "shipment_id")
    private Long shipmentId;
}
