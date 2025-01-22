package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Setter
@Getter
@Table(name = "ti_truck_driver_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class TiTruckDriverDetails extends MultiTenancy {

    @Column(name = "driver_name")
    private String driverName;

    @Column(name = "driver_mobile_number")
    private String driverMobileNumber;

    @Column(name = "truck_number_plate")
    private String truckNumberPlate;

    @Column(name = "trailer_number_plate")
    private String trailerNumberPlate;

    @Column(name = "truck_or_trailer_type")
    private String truckOrTrailerType;

}

