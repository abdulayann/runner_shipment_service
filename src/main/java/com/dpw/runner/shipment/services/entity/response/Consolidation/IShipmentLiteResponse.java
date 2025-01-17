package com.dpw.runner.shipment.services.entity.response.Consolidation;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

public interface IShipmentLiteResponse extends IRunnerResponse {
  Long getConsolId();

  String getShipmentId();

  String getHouseBill();
}
