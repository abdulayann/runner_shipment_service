package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.Map;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SailingInformationResponse implements IRunnerResponse {

    private Long id;
    private String carrierReceiptPlace;
    private String pol;
    private String pod;
    private String carrierDeliveryPlace;
    private String carrier;
    private String vesselName;
    private String voyageNo;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private String scacCode;
    private LocalDateTime earliestDepartureDate;
    private LocalDateTime latestDeliveryDate;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime emptyContainerPickupCutoff;
    private LocalDateTime loadedContainerGateInCutoff;
    private Map<String, Object> unlocationData;
}
