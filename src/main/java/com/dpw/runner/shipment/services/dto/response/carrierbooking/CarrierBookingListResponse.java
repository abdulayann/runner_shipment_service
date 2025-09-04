package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class CarrierBookingListResponse implements IRunnerResponse {
    private Long id;
    private Integer tenantId;

    // Core booking references
    private String bookingNo;            // CR Booking No. (Hyperlink)
    private String carrierBookingNo;     // Carrier Booking No.
    private String consolidationNo;      // Consol No.
    private String shipmentNo;           // Shipment No. (Hyperlink)

    // Statuses
    private String status;               // Status
    private String siStatus;             // SI Status
    private String vgmStatus;            // VGM Status

    // Carrier / vessel info
    private String carrier;              // Carrier
    private String vesselName;               // Vessel
    private String voyageNo;               // Voyage

    // Routing codes (UNLOC)
    private String pol;              // POL Code
    private String pod;              // POD Code
    private String placeOfReceiptCode;   // Place of Receipt Code
    private String placeOfDeliveryCode;  // Place of Delivery Code

    // Routing city display
    private String polCity;              // POL
    private String podCity;              // POD
    private String placeOfReceipt;       // Place of Receipt
    private String placeOfDelivery;      // Place of Delivery

    // Important dates
    private LocalDateTime emptyContainerPickupCutoff;  // Empty Cont. Pickup
    private LocalDateTime loadedContainerGateInCutoff;   // Loaded Cont. Drop
    private LocalDateTime shipInstructionCutoff;              // SI Cutoff
    private LocalDateTime verifiedGrossMassCutoff;             // VGM Cutoff
    private LocalDateTime eta;                   // ETA
    private LocalDateTime etd;                   // ETD

    // Contract / BL
    private String contractNo;           // Contract No.
    private String carrierBLNo;          // Carrier BL No.

    // Parties (for list just names, not full objects)
    private String shipper;              // Shipper
    private String consignee;            // Consignee

    // Comments
    private String bookingComment;       // Truncated view on UI
    private String carrierComment;       // Truncated view on UI
}
