package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class ConsolidationDao implements IConsolidationDetailsDao {
    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails) {
        return consolidationRepository.save(consolidationDetails);
    }

    @Override
    public ConsolidationDetails update(ConsolidationDetails consolidationDetails) {
        validateLockStatus(consolidationDetails.getId());
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
}
