package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "routings")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE routings SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Routings extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "leg")
    private Long leg;

    @Column(name = "mode")
    @MasterData(type = MasterDataType.MODE)
    private String mode;

    @Column(name = "routing_status")
    @MasterData(type = MasterDataType.ROUTING_STATUS)
    private String routingStatus;

    @Column(name = "vessel_name")
    private String vesselName;

    @Column(name = "pol_id")
    private Long polId;

    @Column(name = "pod_id")
    private Long podId;

    @Column(name = "is_domestic")
    private boolean isDomestic;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "ata")
    private LocalDateTime ata;

    @Column(name = "atd")
    private LocalDateTime atd;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "is_linked")
    private Boolean isLinked;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "aircraft_registration")
    private String aircraftRegistration;

    @Column(name = "flight_number")
    private String flightNumber;

    @Column(name = "aircraft_type")
    private String aircraftType;

    @Column(name = "route_leg_id")
    private Long routeLegId;

    @Column(name = "vessel_id")
    private Long vesselId;

    @Column(name = "transit_days")
    private Long transitDays;
}

