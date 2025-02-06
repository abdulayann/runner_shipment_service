package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;

@Entity
@Setter
@Getter
@Table(name = "ti_truck_driver_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE ti_truck_driver_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TiTruckDriverDetails extends MultiTenancy {

    @Column(name = "ti_leg_id")
    private Long tiLegId;

    @Column(name = "driver_name")
    @Size(max=30, message = "max size is 30 for driver_name")
    private String driverName;

    @Column(name = "driver_mobile_number")
    @Size(max=18, message = "max size is 18 for driver_mobile_number")
    private String driverMobileNumber;

    @Column(name = "truck_number_plate")
    @Size(max=20, message = "max size is 20 for truck_number_plate")
    private String truckNumberPlate;

    @Column(name = "trailer_number_plate")
    @Size(max=20, message = "max size is 20 for trailer_number_plate")
    private String trailerNumberPlate;

    @Column(name = "truck_or_trailer_type")
    private String truckOrTrailerType;

}

