package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;

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

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "leg")
    private Long leg;

    @Column(name = "mode")
    @Size(max=4, message = "max size is 4 for mode")
    @MasterData(type = MasterDataType.MODE)
    private String mode;

    @Column(name = "routing_status")
    @MasterData(type = MasterDataType.ROUTING_STATUS)
    private String routingStatus;

    @Column(name = "vessel_name")
    @Size(max=2048, message = "max size is 2048 for vessel_name")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vesselName;

    @Column(name = "pol")
    @UnlocationData
    private String pol;

    @Column(name = "pod")
    @UnlocationData
    private String pod;

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

    @Column(name = "transit_days")
    private Long transitDays;

    @Column(name = "carrier")
    @DedicatedMasterData(type = Constants.CARRIER_MASTER_DATA)
    private String carrier;

    @Column(name = "truck_reference_number")
    private String truckReferenceNumber;

    @Column(name = "carrier_country")
    @Size(max = 32, message = "max size is 32 for carrier_country")
    @MasterData(type = MasterDataType.COUNTRIES)
    private String carrierCountry;

    public boolean getIsDomestic() {
        return isDomestic;
    }
}

