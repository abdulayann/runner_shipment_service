package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "truck_driver_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE truck_driver_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TruckDriverDetails extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Enumerated(EnumType.STRING)
    @Column(name = "transporter_type")
    private Ownership transporterType;

    // String field in the database, selection of organisation on UI
    @Column(name = "transporter_name")
    private String transporterName;

    @Column(name = "driver_name")
    private String driverName;

    @Column(name = "driver_mobile_number")
    private String driverMobileNumber;

    @Column(name = "truck_number_plate")
    private String truckNumberPlate;

    @Column(name = "trailer_number_plate")
    private String trailerNumberPlate;

    @Column(name = "truck_or_trailer_type_id")
    private String truckOrTrailerType;

    @Column(name = "container_type_code")
    private String containerTypeCode;

    @Column(name = "container_id")
    private Long containerId;
    
    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "self_transporter_name")
    private String selfTransporterName;
}
