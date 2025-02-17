package com.dpw.runner.shipment.services.entity.response.consolidation;

import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@NoArgsConstructor
@Builder
public class ConsolidationLiteResponse {
    private Long id;
    private UUID guid;
    private String createdBy;
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String shipmentType;
    private Boolean isDomestic;
    private String payment;
    private LocalDateTime bookingCutoff;
    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime terminalCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime reeferCutoff;
    private String referenceNumber;
    private String bookingStatus;
    private String bookingNumber;
    private String mawb;
    private LocalDateTime eta;
    private LocalDateTime ata;
    private LocalDateTime etd;
    private LocalDateTime atd;
    private String voyage;
    private String shippingLine;
    private Long carrierId;
    private String vessel;
    private String origin;
    private String destination;
    private String originPort;
    private String destinationPort;
    private String originLocCode;
    private String destinationLocCode;
    private String originPortLocCode;
    private String destinationPortLocCode;

    public ConsolidationLiteResponse(Long id, UUID guid, String createdBy, String consolidationNumber,
                                     String consolidationType, String transportMode, String shipmentType, Boolean isDomestic,
                                     String payment, LocalDateTime bookingCutoff, LocalDateTime estimatedTerminalCutoff,
                                     LocalDateTime terminalCutoff, LocalDateTime shipInstructionCutoff,
                                     LocalDateTime hazardousBookingCutoff, LocalDateTime verifiedGrossMassCutoff,
                                     LocalDateTime reeferCutoff, String referenceNumber, String bookingStatus,
                                     String bookingNumber, String mawb, LocalDateTime eta, LocalDateTime ata, LocalDateTime etd, LocalDateTime atd,
                                     String voyage, String shippingLine, Long carrierId, String vessel, String origin, String destination, String originPort, String destinationPort,
                                     String originLocCode, String destinationLocCode, String originPortLocCode, String destinationPortLocCode) {
        this.id = id;
        this.guid = guid;
        this.createdBy = createdBy;
        this.consolidationNumber = consolidationNumber;
        this.consolidationType = consolidationType;
        this.transportMode = transportMode;
        this.shipmentType = shipmentType;
        this.isDomestic = isDomestic;
        this.payment = payment;
        this.bookingCutoff = bookingCutoff;
        this.estimatedTerminalCutoff = estimatedTerminalCutoff;
        this.terminalCutoff = terminalCutoff;
        this.shipInstructionCutoff = shipInstructionCutoff;
        this.hazardousBookingCutoff = hazardousBookingCutoff;
        this.verifiedGrossMassCutoff = verifiedGrossMassCutoff;
        this.reeferCutoff = reeferCutoff;
        this.referenceNumber = referenceNumber;
        this.bookingStatus = bookingStatus;
        this.bookingNumber = bookingNumber;
        this.mawb = mawb;
        this.eta = eta;
        this.ata = ata;
        this.etd = etd;
        this.atd = atd;
        this.voyage = voyage;
        this.shippingLine = shippingLine;
        this.carrierId = carrierId;
        this.vessel = vessel;
        this.origin = origin;
        this.destination = destination;
        this.originPort = originPort;
        this.destinationPort = destinationPort;
        this.originLocCode = originLocCode;
        this.destinationLocCode = destinationLocCode;
        this.originPortLocCode = originPortLocCode;
        this.destinationPortLocCode = destinationPortLocCode;
    }
}
