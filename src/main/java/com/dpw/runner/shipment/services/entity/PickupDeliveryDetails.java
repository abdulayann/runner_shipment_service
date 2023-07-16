package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

@Entity
@Setter
@Getter
@Table(name = "pickup_delivery_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE pickup_delivery_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
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

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "transporter_id", referencedColumnName = "id")
    private Parties transporterDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "broker_id", referencedColumnName = "id")
    private Parties brokerDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "destination_id", referencedColumnName = "id")
    private Parties destinationDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "source_id", referencedColumnName = "id")
    private Parties sourceDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "agent_id", referencedColumnName = "id")
    private Parties agentDetail;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
