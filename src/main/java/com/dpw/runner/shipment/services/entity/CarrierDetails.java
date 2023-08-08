package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.GenerationTime;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "carrier_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE carrier_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CarrierDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Column(name = "shipping_line")
    @DedicatedMasterData(type = Constants.CARRIER_MASTER_DATA)
    private String shippingLine;

    @Column(name = Constants.VESSEL_MASTER_DATA)
    @DedicatedMasterData(type = "Vessel")
    private String vessel;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "flight_number")
    private String flightNumber;

    @Column(name = "aircraft_type")
    @MasterData(type = MasterDataType.AIRCRAFT_TYPE)
    private String aircraftType;

    @Column(name = "aircraft_registration")
    private String aircraftRegistration;

    @Column(name = "truck_ref_number")
    private String truckRefNumber;

    @Column(name = "journey_number")
    private String journeyNumber;

    @Column(name = "journey_ref_number")
    private String journeyRefNumber;

    @Column(name = "origin")
    @UnlocationData
    private String origin;

    @Column(name = "destination")
    @UnlocationData
    private String destination;

    @Column(name = "origin_port")
    @UnlocationData
    private String originPort;

    @Column(name = "destination_port")
    @UnlocationData
    private String destinationPort;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "ata")
    private LocalDateTime ata;

    @Column(name = "atd")
    private LocalDateTime atd;
}
