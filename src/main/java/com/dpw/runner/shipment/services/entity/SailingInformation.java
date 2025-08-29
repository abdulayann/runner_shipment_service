package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.Map;

@Entity
@Table(name = "sailing_information")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE sailing_information SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class SailingInformation extends MultiTenancy {

    @UnlocationData
    @Column(name = "carrier_receipt_place")
    private String carrierReceiptPlace;

    @Column(name = "pol")   // Port of Loading
    @UnlocationData
    private String pol;

    @Column(name = "pod")   // Port of Discharge
    @UnlocationData
    private String pod;

    @Column(name = "carrier_delivery_place")
    @UnlocationData
    private String carrierDeliveryPlace;

    @Column(name = "carrier")
    @DedicatedMasterData(type = Constants.CARRIER_MASTER_DATA)
    private String carrier;

    @Column(name = "vessel_name")
    @Size(max=2048, message = "max size is 2048 for vessel_name")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vesselName;

    @Column(name = "voyage_no")
    private String voyageNo;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "earliest_departure_date")
    private LocalDateTime earliestDepartureDate;

    @Column(name = "latest_delivery_date")
    private LocalDateTime latestDeliveryDate;

    @Column(name = "terminal_cutoff")
    private LocalDateTime terminalCutoff;

    @Column(name = "verified_gross_mass_cutoff")
    private LocalDateTime verifiedGrossMassCutoff;

    @Column(name = "ship_instruction_cutoff")
    private LocalDateTime shipInstructionCutoff;

    @Column(name = "hazardous_booking_cutoff")
    private LocalDateTime hazardousBookingCutoff;

    @Column(name = "reefer_cutoff")
    private LocalDateTime reeferCutoff;

    @Column(name = "empty_cont_pickup_cutoff")
    private LocalDateTime emptyContainerPickupCutoff;

    @Column(name = "loaded_cont_gate_in_cutoff")
    private LocalDateTime loadedContainerGateInCutoff;

    @Column(name = "last_updated_sailing_info")
    private LocalDateTime lastUpdatedSailingInfo;

    @Type(type = "jsonb")
    @Column(name = "sailing_schedule_data", columnDefinition = "jsonb")
    private Map<String, Object> sailingScheduleData;
}
