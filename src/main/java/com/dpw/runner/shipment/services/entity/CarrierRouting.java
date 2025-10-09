package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.time.LocalDateTime;

@Entity
@Table(name = "carrier_routings")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE carrier_routings SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CarrierRouting extends MultiTenancy {

    @Column(name = "sequence")
    private Integer sequence;

    @Column(name = "transport_mode")
    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportMode;

    @Column(name = "carriage_type", length = 50)
    @Enumerated(EnumType.STRING)
    private RoutingCarriage carriageType;

    @Column(name = "pol")   // Port of Loading
    @UnlocationData
    private String pol;

    @Column(name = "pod")   // Port of Discharge
    @UnlocationData
    private String pod;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "vessel_name")
    @Size(max=2048, message = "max size is 2048 for vessel_name")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vesselName;

    @Column(name = "voyage_no")
    private String voyageNo;

    @Column(name = "carrier_booking_id")
    private Long carrierBookingId;
}

