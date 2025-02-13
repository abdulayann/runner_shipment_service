package com.dpw.runner.shipment.services.entity.response.consolidation;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.time.LocalDateTime;
import java.util.List;

public interface IConsolidationDetailsResponse extends IRunnerResponse {
    Long getId();

    String getCreatedBy();

    String getConsolidationNumber();

    String getConsolidationType();

    String getTransportMode();

    String getShipmentType();

    String getVoyage();

    String getShippingLine();

    LocalDateTime getEta();

    LocalDateTime getEtd();

    LocalDateTime getAta();

    LocalDateTime getAtd();

    List<IContainerLiteResponse> getContainersList();

    String getReferenceNumber();

    String getBookingStatus();

    String getBookingNumber();

    String getMawb();

    List<IShipmentLiteResponse> getShipmentsList();

    Boolean getDomestic();

    String getPayment();

    LocalDateTime getBookingCutoff();

    LocalDateTime getShipInstructionCutoff();

    LocalDateTime getHazardousBookingCutoff();

    LocalDateTime getEstimatedTerminalCutoff();

    LocalDateTime getTerminalCutoff();

    LocalDateTime getVerifiedGrossMassCutoff();

    LocalDateTime getReeferCutoff();
}
