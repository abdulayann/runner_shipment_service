package com.dpw.runner.shipment.services.entity.response;

import java.time.LocalDateTime;

public interface ICarrierDetailsResponse {

  String getVoyage();

  String getShippingLine();

  LocalDateTime getEta();

  LocalDateTime getEtd();

  LocalDateTime getAta();

  LocalDateTime getAtd();
}
