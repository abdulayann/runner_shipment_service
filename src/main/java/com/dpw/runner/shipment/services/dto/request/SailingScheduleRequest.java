package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SailingScheduleRequest {

    private Long id;
    private Long shipmentId;
    private String scac;
    private String carrierName;
    private String serviceName;
    private String vesselName;
    private String vesselDisplayName;
    private String vesselMmsi;
    private String voyageNumber;
    private String imoNumber;

    private String originUnlocReferenceGuid;
    private String originUnloc;
    private String originCountry;
    private String originCityName;
    private String originSubdivision;
    private String originTerminal;

    private String destinationUnloc;
    private String destinationUnlocReferenceGuid;
    private String destinationCountry;
    private String destinationSubdivision;
    private String destinationCityName;
    private String destinationTerminal;

    private LocalDateTime originEstimatedDepartureDate;
    private LocalDateTime originActualDepartureDate;
    private LocalDateTime destinationEstimatedArrivalDate;
    private LocalDateTime destinationActualArrivalDate;

    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime terminalCutoff;
    private LocalDateTime bookingCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime latestFullEquipmentDelivery;
    private LocalDateTime earliestFullEquipmentDropoffDate;
    private LocalDateTime earliestEmptyEquipmentPickupDate;

    private Integer totalDuration;
    private String scheduleType;
    private LocalDateTime insertDate;
    private String insertUserId;
    private LocalDateTime updateDate;
    private String updateUserId;
    private String source;
    private String vesselId;
    private String vesselGuid;

    private List<SailingScheduleLeg> sailingScheduleLegs;

    private String guid;
}
