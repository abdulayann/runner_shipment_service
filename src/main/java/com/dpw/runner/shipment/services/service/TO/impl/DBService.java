package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.entity.IntegrationEntity;
import com.dpw.runner.shipment.services.entity.ResponseEntity;
import com.dpw.runner.shipment.services.repository.interfaces.IntegrationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ResponseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class DBService {

    @Autowired
    IntegrationRepository integrationRepository;

    @Autowired
    ResponseRepository responseRepository;
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
