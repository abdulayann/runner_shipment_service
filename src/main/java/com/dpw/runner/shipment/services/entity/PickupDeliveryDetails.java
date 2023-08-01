package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
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

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "transporter_id", referencedColumnName = "id")
    @BatchSize(size = 50)
    private Parties transporterDetail;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "broker_id", referencedColumnName = "id")
    @BatchSize(size = 50)
    private Parties brokerDetail;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "destination_id", referencedColumnName = "id")
    @BatchSize(size = 50)
    private Parties destinationDetail;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "source_id", referencedColumnName = "id")
    @BatchSize(size = 50)
    private Parties sourceDetail;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "agent_id", referencedColumnName = "id")
    @BatchSize(size = 50)
    private Parties agentDetail;

    @Column(name = "drop_mode")
    private String dropMode;

    @Column(name = "labour_charge")
    private BigDecimal labourCharge;

    @Column(name = "labour_charge_unit")
    private String labourChargeUnit;

    @Column(name = "labour_duration")
    private LocalTime labourDuration;

    @Column(name = "shipper_ref")
    private String shipperRef;

    @Column(name = "interim_receipt")
    private String interimReceipt;

    @Column(name = "fcl_available_date")
    private LocalDateTime fclAvailableDate;

    @Column(name = "storage_date")
    private LocalDateTime storageDate;

    @Column(name = "truck_wait_time_charge")
    private BigDecimal truckWaitTimeCharge;

    @Column(name = "truck_wait_time_charge_unit")
    private String truckWaitTimeChargeUnit;

    @Column(name = "truck_wait_duration")
    private LocalTime truckWaitDuration;

    @Column(name = "storage_charge")
    private BigDecimal storageCharge;

    @Column(name = "storage_charge_unit")
    private String storageChargeUnit;

    @Column(name = "storage_charge_duration")
    private LocalTime storageChargeDuration;

    @Column(name = "ucr_reference")
    public String ucrReference;

    @Column(name = "empty_truck_in_date")
    public LocalDateTime emptyTruckInDate;

    @Column(name = "loaded_truck_gate_out_date")
    public LocalDateTime loadedTruckGateOutDate;
}
