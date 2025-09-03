package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;

@Entity
@Setter
@Getter
@Table(name = "truck_driver_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE truck_driver_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TruckDriverDetails extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Enumerated(EnumType.STRING)
    @Column(name = "transporter_type")
    private Ownership transporterType;

    // String field in the database, selection of organisation on UI
    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "third_party_transporter", referencedColumnName = "id")
    @OrganizationData
    private Parties thirdPartyTransporter;

    @Column(name = "transporter_name")
    private String transporterName;

    @Column(name = "driver_name")
    private String driverName;

    @Column(name = "driver_mobile_number")
    @Size(max=25, message = "max size is 25 for driver_mobile_number")
    private String driverMobileNumber;

    @Column(name = "truck_number_plate")
    @Size(max=50, message = "max size is 50 for truck_number_plate")
    private String truckNumberPlate;

    @Column(name = "trailer_number_plate")
    @Size(max=50, message = "max size is 50 for trailer_number_plate")
    private String trailerNumberPlate;

    @Column(name = "truck_or_trailer_type_id")
    @Size(max=50, message = "max size is 50 for truck_or_trailer_type_id")
    @MasterData(type = MasterDataType.TRUCK_TYPE)
    private String truckOrTrailerType;

    @Column(name = "container_type_code")
    @Size(max=20, message = "max size is 20 for container_type_code")
    private String containerTypeCode;

    @Column(name = "container_id")
    private Long containerId;
    
    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "self_transporter_name")
    private String selfTransporterName;

    @Column(name = "remarks")
    private String remarks;

    @Column(name = "truck_status")
    private String truckStatus;

    @Column(name = "driver_id")
    @Size(max=50, message = "max size is 50 for driver_id")
    private String driverId;
}
