package com.dpw.runner.shipment.services.entity.response.impl;

import com.dpw.runner.shipment.services.entity.response.ICarrierDetailsResponse;
import com.dpw.runner.shipment.services.entity.response.IConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.entity.response.IContainersResponse;
import com.dpw.runner.shipment.services.entity.response.IShipmentResponse;
import java.time.LocalDateTime;
import java.util.List;

public class ConsolidationDetailsResponseImpl implements IConsolidationDetailsResponse {
  private Long id;
  private String consolidationNumber;
  private String consolidationType;
  private String transportMode;
  private String shipmentType;
  private Boolean isDomestic;
  private String createdBy;
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
  private ICarrierDetailsResponse carrierDetails;
  private List<IContainersResponse> containersList;
  private List<IShipmentResponse> shipmentsList;

  public ConsolidationDetailsResponseImpl(Long id, String consolidationNumber,
      String consolidationType, String transportMode, String shipmentType, Boolean isDomestic,
      String createdBy, String payment, LocalDateTime bookingCutoff,
      LocalDateTime estimatedTerminalCutoff, LocalDateTime terminalCutoff,
      LocalDateTime shipInstructionCutoff, LocalDateTime hazardousBookingCutoff,
      LocalDateTime verifiedGrossMassCutoff, LocalDateTime reeferCutoff, String referenceNumber,
      String bookingStatus, String bookingNumber, String mawb,
      ICarrierDetailsResponse carrierDetails, List<IContainersResponse> containersList,
      List<IShipmentResponse> shipmentsList) {
    this.id = id;
    this.consolidationNumber = consolidationNumber;
    this.consolidationType = consolidationType;
    this.transportMode = transportMode;
    this.shipmentType = shipmentType;
    this.isDomestic = isDomestic;
    this.createdBy = createdBy;
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
    this.carrierDetails = carrierDetails;
    this.containersList = containersList;
    this.shipmentsList = shipmentsList;
  }

  @Override
  public Long getId() {
    return id;
  }

  @Override
  public String getCreatedBy() {
    return createdBy;
  }

  @Override
  public String getConsolidationNumber() {
    return consolidationNumber;
  }

  @Override
  public String getConsolidationType() {
    return consolidationType;
  }

  @Override
  public String getTransportMode() {
    return transportMode;
  }

  @Override
  public String getShipmentType() {
    return shipmentType;
  }

  @Override
  public String getVoyage() {
    return null;
  }

  @Override
  public String getShippingLine() {
    return null;
  }

  @Override
  public LocalDateTime getEta() {
    return null;
  }

  @Override
  public LocalDateTime getEtd() {
    return null;
  }

  @Override
  public LocalDateTime getAta() {
    return null;
  }

  @Override
  public LocalDateTime getAtd() {
    return null;
  }

  @Override
  public ICarrierDetailsResponse getCarrierDetails() {
    return carrierDetails;
  }

  @Override
  public List<IContainersResponse> getContainersList() {
    return containersList;
  }

  @Override
  public String getReferenceNumber() {
    return referenceNumber;
  }

  @Override
  public String getBookingStatus() {
    return bookingStatus;
  }

  @Override
  public String getBookingNumber() {
    return bookingNumber;
  }

  @Override
  public String getMawb() {
    return mawb;
  }

  @Override
  public List<IShipmentResponse> getShipmentsList() {
    return shipmentsList;
  }

  @Override
  public Boolean getDomestic() {
    return isDomestic;
  }

  @Override
  public String getPayment() {
    return payment;
  }

  @Override
  public LocalDateTime getBookingCutoff() {
    return bookingCutoff;
  }

  @Override
  public LocalDateTime getShipInstructionCutoff() {
    return shipInstructionCutoff;
  }

  @Override
  public LocalDateTime getHazardousBookingCutoff() {
    return hazardousBookingCutoff;
  }

  @Override
  public LocalDateTime getEstimatedTerminalCutoff() {
    return estimatedTerminalCutoff;
  }

  @Override
  public LocalDateTime getTerminalCutoff() {
    return terminalCutoff;
  }

  @Override
  public LocalDateTime getVerifiedGrossMassCutoff() {
    return verifiedGrossMassCutoff;
  }

  @Override
  public LocalDateTime getReeferCutoff() {
    return reeferCutoff;
  }
}
