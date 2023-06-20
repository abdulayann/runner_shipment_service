package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.experimental.Accessors;
import lombok.*;

import javax.persistence.*;
import java.time.Duration;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "shipment_services")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentServices extends BaseEntity {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "service_type")
    private String serviceType;

    @Column(name = "contractor_id")
    private Long contractorId;

    @Column(name = "contractor_address_id")
    private Long contractorAddressId;

    @Column(name = "srv_location")
    private int srvLocation;

    @Column(name = "booking_date")
    private LocalDateTime bookingDate;

    @Column(name = "service_count")
    private String serviceCount;

    @Column(name = "service_duration")
    private Duration serviceDuration;

    @Column(name = "completion_date")
    private LocalDateTime completionDate;

    @Column(name = "ref_number")
    private String refNumber;

    @Column(name = "service_notes")
    private String serviceNotes;
}
