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
import java.time.LocalDateTime;


@Entity
@Data
@Table(name = "booking_carriage")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE booking_carriage SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class BookingCarriage extends MultiTenancy {

    @Column(name = "port_of_loading")
    @UnlocationData
    private String portOfLoading;

    @Column(name = "port_of_discharge")
    @UnlocationData
    private String portOfDischarge;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "vessel")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vessel;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "carriage_type")
    @MasterData(type = MasterDataType.CARRIAGE_TYPE)
    private String carriageType;

    @Column(name = "carriage_mode")
    @MasterData(type = MasterDataType.CARRIAGE_MODE)
    private String carriageMode;

    @Column(name = "shipment_id")
    private Long shipmentId;

}
