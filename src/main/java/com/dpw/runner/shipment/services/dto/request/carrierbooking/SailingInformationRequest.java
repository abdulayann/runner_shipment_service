package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SailingInformationRequest {
    private String carrierReceiptPlace;
    private String pol;   // Port of Loading
    private String pod;   // Port of Discharge
    private String carrierDeliveryPlace;
    private String carrier;
    private String vesselName;
    private String voyageNo;
    @ExcludeTimeZone
    private LocalDateTime eta;
    @ExcludeTimeZone
    private LocalDateTime etd;
    private LocalDateTime earliestDepartureDate;
    private LocalDateTime latestDeliveryDate;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime emptyContainerPickupCutoff;
    private LocalDateTime loadedContainerGateInCutoff;
    private LocalDateTime lastUpdatedSailingInfo;
}
