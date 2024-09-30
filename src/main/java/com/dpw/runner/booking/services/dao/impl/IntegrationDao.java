package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.booking.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.booking.services.entity.IntegrationResponse;
import com.dpw.runner.booking.services.repository.interfaces.IIntegrationRespsonseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class IntegrationDao implements IIntegrationResponseDao {
    @Autowired
    private IIntegrationRespsonseRepository integrationRespsonseRepository;

    @Override
    public IntegrationResponse save(IntegrationResponse integrationResponse) {
        return integrationRespsonseRepository.save(integrationResponse);
    }

    @Override
    public List<IntegrationResponse> getIntegrationResponses(IntegrationResponseRequest request) {
        return integrationRespsonseRepository.findByEntityIdAndEntityType(request.getEntityId(), request.getEntityType()).get();
    }
}
