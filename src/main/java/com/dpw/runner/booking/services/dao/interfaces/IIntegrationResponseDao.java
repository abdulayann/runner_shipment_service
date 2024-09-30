package com.dpw.runner.booking.services.dao.interfaces;

import com.dpw.runner.booking.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.booking.services.entity.IntegrationResponse;

import java.util.List;

public interface IIntegrationResponseDao {

    IntegrationResponse save(IntegrationResponse integrationResponse);
    List<IntegrationResponse> getIntegrationResponses(IntegrationResponseRequest request);
}
