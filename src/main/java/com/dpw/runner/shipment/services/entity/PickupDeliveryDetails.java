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

    @Column(name = "type")
    private String type;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @OneToOne(targetEntity = Parties.class)
    @JoinColumn(name = "transporter_id", referencedColumnName = "id")
    private Parties transporterDetail;

    @OneToOne(targetEntity = Parties.class)
    @JoinColumn(name = "broker_id", referencedColumnName = "id")
    private Parties brokerDetail;

    @OneToOne(targetEntity = Parties.class)
    @JoinColumn(name = "destination_id", referencedColumnName = "id")
    private Parties destinationDetail;

    @OneToOne(targetEntity = Parties.class)
    @JoinColumn(name = "source_id", referencedColumnName = "id")
    private Parties sourceDetail;

    @OneToOne(targetEntity = Parties.class)
    @JoinColumn(name = "agent_id", referencedColumnName = "id")
    private Parties agentDetail;


}
