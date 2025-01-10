package com.dpw.runner.shipment.services.entity.response.impl;

import com.dpw.runner.shipment.services.entity.response.ICarrierDetailsResponse;
import java.time.LocalDateTime;

public class CarrierDetailsResponseImpl implements ICarrierDetailsResponse {

  private String voyage;

  private String shippingLine;

  private LocalDateTime eta;

  private LocalDateTime etd;

  private LocalDateTime ata;

  private LocalDateTime atd;

  public CarrierDetailsResponseImpl(String voyage, String shippingLine, LocalDateTime eta,
      LocalDateTime etd, LocalDateTime ata, LocalDateTime atd) {
    this.voyage = voyage;
    this.shippingLine = shippingLine;
    this.eta = eta;
    this.etd = etd;
    this.ata = ata;
    this.atd = atd;
  }

  @Override
  public String getVoyage() {
    return voyage;
  }

  @Override
  public String getShippingLine() {
    return shippingLine;
  }

  @Override
  public LocalDateTime getEta() {
    return eta;
  }

  @Override
  public LocalDateTime getEtd() {
    return etd;
  }

  @Override
  public LocalDateTime getAta() {
    return ata;
  }

  @Override
  public LocalDateTime getAtd() {
    return atd;
  }
}
