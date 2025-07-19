package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.dto.request.SailingScheduleLeg;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Getter
@Setter
@Table(name = "sailing_schedule")
@AllArgsConstructor
@NoArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class SailingSchedule extends BaseEntity {

        @Column(name = "shipment_id")
        private Long shipmentId;

        @Column(name = "scac")
        private String scac;

        @Column(name = "carrier_name")
        private String carrierName;

        @Column(name = "service_name")
        private String serviceName;

        @Column(name = "vessel_name")
        private String vesselName;

        @Column(name = "vessel_display_name")
        private String vesselDisplayName;

        @Column(name = "vessel_mmsi")
        private String vesselMmsi;

        @Column(name = "voyage_number")
        private String voyageNumber;

        @Column(name = "imo_number")
        private String imoNumber;

        @Column(name = "origin_unloc_reference_guid")
        private String originUnlocReferenceGuid;

        @Column(name = "origin_unloc")
        private String originUnloc;

        @Column(name = "origin_country")
        private String originCountry;

        @Column(name = "origin_city_name")
        private String originCityName;

        @Column(name = "origin_subdivision")
        private String originSubdivision;

        @Column(name = "origin_terminal")
        private String originTerminal;

        @Column(name = "destination_unloc")
        private String destinationUnloc;

        @Column(name = "destination_unloc_reference_guid")
        private String destinationUnlocReferenceGuid;

        @Column(name = "destination_country")
        private String destinationCountry;

        @Column(name = "destination_subdivision")
        private String destinationSubdivision;

        @Column(name = "destination_city_name")
        private String destinationCityName;

        @Column(name = "destination_terminal")
        private String destinationTerminal;

        @Column(name = "origin_estimated_departure_date")
        private LocalDateTime originEstimatedDepartureDate;

        @Column(name = "origin_actual_departure_date")
        private LocalDateTime originActualDepartureDate;

        @Column(name = "destination_estimated_arrival_date")
        private LocalDateTime destinationEstimatedArrivalDate;

        @Column(name = "destination_actual_arrival_date")
        private LocalDateTime destinationActualArrivalDate;

        @Column(name = "estimated_terminal_cutoff")
        private LocalDateTime estimatedTerminalCutoff;

        @Column(name = "terminal_cutoff")
        private LocalDateTime terminalCutoff;

        @Column(name = "booking_cutoff")
        private LocalDateTime bookingCutoff;

        @Column(name = "ship_instruction_cutoff")
        private LocalDateTime shipInstructionCutoff;

        @Column(name = "hazardous_booking_cutoff")
        private LocalDateTime hazardousBookingCutoff;

        @Column(name = "verified_gross_mass_cutoff")
        private LocalDateTime verifiedGrossMassCutoff;

        @Column(name = "reefer_cutoff")
        private LocalDateTime reeferCutoff;

        @Column(name = "latest_full_equipment_delivery")
        private LocalDateTime latestFullEquipmentDelivery;

        @Column(name = "earliest_full_equipment_dropoff_date")
        private LocalDateTime earliestFullEquipmentDropoffDate;

        @Column(name = "earliest_empty_equipment_pickup_date")
        private LocalDateTime earliestEmptyEquipmentPickupDate;

        @Column(name = "total_duration")
        private Integer totalDuration;

        @Column(name = "schedule_type")
        private String scheduleType;

        @Column(name = "insert_date")
        private LocalDateTime insertDate;

        @Column(name = "insert_user_id")
        private String insertUserId;

        @Column(name = "update_date")
        private LocalDateTime updateDate;

        @Column(name = "update_user_id")
        private String updateUserId;

        @Column(name = "source")
        private String source;

        @Column(name = "vessel_id")
        private String vesselId;

        @Column(name = "vessel_guid")
        private UUID vesselGuid;

        // JSON column for legs
        @Type(type = "jsonb")
        @Column(name = "sailing_schedule_legs", columnDefinition = "jsonb")
        private List<SailingScheduleLeg> sailingScheduleLegs;

        // Note: Assuming scheduleHistories is a placeholder; no @Column on collection fields.
        @Transient
        private List<Object> scheduleHistories;
}
