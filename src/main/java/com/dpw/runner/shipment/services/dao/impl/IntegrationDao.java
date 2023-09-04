package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IIntegrationRespsonseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class IntegrationDao implements IIntegrationResponseDao {
    @Autowired
    private IIntegrationRespsonseRepository integrationRespsonseRepository;

    @Override
    public IntegrationResponse save(IntegrationResponse integrationResponse) {
        return integrationRespsonseRepository.save(integrationResponse);
    }
}
