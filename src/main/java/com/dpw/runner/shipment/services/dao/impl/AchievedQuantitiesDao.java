package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAchievedQuantitiesDao;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.repository.interfaces.IAchievedQuantitiesRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@Slf4j
public class AchievedQuantitiesDao implements IAchievedQuantitiesDao {

    @Autowired
    private IAchievedQuantitiesRepository achievedQuantitiesRepository;

    @Override
    public AchievedQuantities save(AchievedQuantities achievedQuantities) {
        return achievedQuantitiesRepository.save(achievedQuantities);
    }

    @Override
    public Page<AchievedQuantities> findAll(Specification<AchievedQuantities> spec, Pageable pageable) {
        return achievedQuantitiesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<AchievedQuantities> findById(Long id) {
        return achievedQuantitiesRepository.findById(id);
    }

    @Override
    public void delete(AchievedQuantities achievedQuantities) {
        achievedQuantitiesRepository.delete(achievedQuantities);
    }

    public AchievedQuantities updateEntityFromConsolidation(AchievedQuantities achievedQuantities, Long consolidationId) throws RunnerException {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (achievedQuantities.getId() != null) {
                long id = achievedQuantities.getId();
                Optional<AchievedQuantities> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Achieved Quantities is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            achievedQuantities = save(achievedQuantities);
            return achievedQuantities;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public AchievedQuantities updateEntityFromShipmentConsole(AchievedQuantities achievedQuantities) throws RunnerException {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (achievedQuantities.getId() != null) {
                long id = achievedQuantities.getId();
                Optional<AchievedQuantities> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Achieved Quantities is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            achievedQuantities = save(achievedQuantities);
            return achievedQuantities;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
