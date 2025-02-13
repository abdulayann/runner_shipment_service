package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.ExcludeAuditLog;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Formula;
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
@Table(name = "carrier_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE carrier_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
@BatchSize(size = 50)
public class CarrierDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;
    @Size(max = 64, message = "max size is 64 for shipping line")
    @Column(name = "shipping_line")
    @DedicatedMasterData(type = Constants.CARRIER_MASTER_DATA)
    private String shippingLine;

    @Column(name = "vessel")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vessel;

    @Column(name = "voyage")
    @Size(max = 20, message = "max size is 20 for voyage")
    private String voyage;

    @Column(name = "flight_number")
    private String flightNumber;

    @Size(max = 20, message = "max size is 20 for aircraft_type")
    @Column(name = "aircraft_type")
    @MasterData(type = MasterDataType.AIRCRAFT_TYPE)
    private String aircraftType;

    @Size(max = 20, message = "max size is 20 for aircraft_type")
    @Column(name = "aircraft_registration")
    private String aircraftRegistration;

    @Column(name = "truck_ref_number")
    private String truckRefNumber;

    @Size(max = 10, message = "max size is 10 for journey_number")
    @Column(name = "journey_number")
    private String journeyNumber;

    @Size(max = 10, message = "max size is 10 for journey_ref_number")
    @Column(name = "journey_ref_number")
    private String journeyRefNumber;

    @Size(max = 100, message = "max size is 100 for origin")
    @Column(name = "origin")
    @UnlocationData
    private String origin;

    @Size(max = 100, message = "max size is 100 for destination")
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

    @Column(name = "vessel_berthing_date")
    private LocalDateTime vesselBerthingDate;

    @ExcludeAuditLog
    @Formula("CONCAT_WS( ' ', voyage, flight_number ) ")
    private String voyageOrFlightNumber;

    @Column(name = "carrier_country")
    @Size(max = 32, message = "max size is 32 for carrier_country")
    @MasterData(type = MasterDataType.COUNTRIES)
    private String carrierCountry;

    @Column(name = "min_transit_hours")
    @Size(max = 15, message = "max size is 15 for min_transit_hours")
    private String minTransitHours;

    @Column(name = "max_transit_hours")
    @Size(max = 15, message = "max size is 15 for max_transit_hours")
    private String maxTransitHours;

    @Column(name = "carrier_added_from_npm")
    private Boolean carrierAddedFromNpm;

    @Column(name = "is_carrier_changed")
    private Boolean isCarrierChanged;

    @Size(max = 100, message = "max size is 100 for cfs")
    @Column(name = "cfs")
    @UnlocationData
    private String cfs;

    @Size(max = 64, message = "max size is 64 for origin_loc_code")
    @Column(name = "origin_loc_code")
    private String originLocCode;

    @Size(max = 64, message = "max size is 64 for destination_loc_code")
    @Column(name = "destination_loc_code")
    private String destinationLocCode;

    @Size(max = 64, message = "max size is 64 for origin_port_loc_code")
    @Column(name = "origin_port_loc_code")
    private String originPortLocCode;

    @Size(max = 64, message = "max size is 64 for destination_port_loc_code")
    @Column(name = "destination_port_loc_code")
    private String destinationPortLocCode;
}
