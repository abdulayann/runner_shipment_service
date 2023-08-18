package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class ConsolidationDao implements IConsolidationDetailsDao {
    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Autowired
    IShipmentRepository shipmentRepository;

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails) {
        if(consolidationDetails.getId() != null) {
            long id = consolidationDetails.getId();
            Optional<ConsolidationDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {}", consolidationDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(consolidationDetails.getShipmentsList() == null) {
                consolidationDetails.setShipmentsList(oldEntity.get().getShipmentsList());
            }
        }
        return consolidationRepository.save(consolidationDetails);
    }

    @Override
    public ConsolidationDetails update(ConsolidationDetails consolidationDetails) {
        validateLockStatus(consolidationDetails.getId());
        if(consolidationDetails.getId() != null) {
            long id = consolidationDetails.getId();
            ConsolidationDetails oldEntity = findById(id).get();
            if(consolidationDetails.getShipmentsList() == null) {
                consolidationDetails.setShipmentsList(oldEntity.getShipmentsList());
            }
        }
        return consolidationRepository.save(consolidationDetails);
    }

    @Override
    public Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable) {
        return consolidationRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ConsolidationDetails> findById(Long id) {
        return consolidationRepository.findById(id);
    }

    @Override
    public void delete(ConsolidationDetails consolidationDetails) {
        validateLockStatus(consolidationDetails.getId());
        consolidationRepository.delete(consolidationDetails);
    }

    private void validateLockStatus(Long id) throws ValidationException {
        Optional<ConsolidationDetails> existingConsolidation = findById(id);
        if(existingConsolidation.get().getIsLocked() != null && existingConsolidation.get().getIsLocked()) {
            throw new ValidationException(ConsolidationConstants.CONSOLIDATION_LOCKED);
        }
    }
    
    public List<ConsolidationDetails> saveAll(List<ConsolidationDetails> consolidationDetails)
    {
        List<ConsolidationDetails> res = new ArrayList<>();
        for(ConsolidationDetails req : consolidationDetails){
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public Optional<ShipmentDetails> findShipmentById(Long shipmentId) {
        return shipmentRepository.findById(shipmentId);
    }

    public Optional<ConsolidationDetails> findByGuid (UUID guid) {
        return consolidationRepository.findByGuid(guid);
    }
}
