package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.entityTO.IntegrationEntity;
import com.dpw.runner.shipment.services.entityTO.ResponseEntity;
import com.dpw.runner.shipment.services.repositoryTO.IntegrationRepository;
import com.dpw.runner.shipment.services.repositoryTO.ResponseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import javax.persistence.EntityManager;
import java.util.List;

@Service
public class DBService {

    @Autowired
   // @Qualifier("secondaryEntityManagerFactory")
    IntegrationRepository integrationRepository;

    @Autowired
   // @Qualifier("secondaryEntityManagerFactory")
    ResponseRepository responseRepository;

    private final EntityManager entityManager;

    @Autowired
    public DBService(@Qualifier("secondaryEntityManagerFactory") EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public IntegrationEntity saveIntegration(IntegrationEntity integrationEntity) {
        return integrationRepository.save(integrationEntity);
    }

    public ResponseEntity createStatus(ResponseEntity statusEntity) {
        return responseRepository.save(statusEntity);
    }


    public ResponseEntity saveIntegrationResponse(ResponseEntity er) {
        return responseRepository.save(er);
    }

    public IntegrationEntity getByUniqueId(String conversationID) {
        return integrationRepository.findByUniqueId(conversationID);
    }

    public List<IntegrationEntity> getByAwbNumber(String awbNumber) {
        return integrationRepository.findByAwbNumber(awbNumber);
    }
}
