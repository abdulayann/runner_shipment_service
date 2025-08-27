package com.dpw.runner.shipment.services.dto.request.carrierbooking;

import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Size;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SailingInformationRequest {

    @UnlocationData
    private String carrierReceiptPlace;

    @UnlocationData
    private String pol;   // Port of Loading

    @UnlocationData
    private String pod;   // Port of Discharge

    @UnlocationData
    private String carrierDeliveryPlace;

    @DedicatedMasterData(type = Constants.CARRIER_MASTER_DATA)
    private String carrier;

    @Size(max=2048, message = "max size is 2048 for vessel_name")
    @DedicatedMasterData(type = Constants.VESSEL_MASTER_DATA)
    private String vesselName;

    private String voyageNo;

    private LocalDateTime eta;
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
}
