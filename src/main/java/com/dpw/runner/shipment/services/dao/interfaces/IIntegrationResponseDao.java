package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;

import java.util.List;

public interface IIntegrationResponseDao {

    IntegrationResponse save(IntegrationResponse integrationResponse);

    List<IntegrationResponse> getIntegrationResponses(IntegrationResponseRequest request);
}
