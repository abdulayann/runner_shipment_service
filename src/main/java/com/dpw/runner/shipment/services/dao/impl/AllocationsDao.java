package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAllocationsDao;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.repository.interfaces.IAllocationsRepository;
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
public class AllocationsDao implements IAllocationsDao {

    @Autowired
    private IAllocationsRepository allocationsRepository;

    @Override
    public Allocations save(Allocations allocations) {
        return allocationsRepository.save(allocations);
    }

    @Override
    public Page<Allocations> findAll(Specification<Allocations> spec, Pageable pageable) {
        return allocationsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Allocations> findById(Long id) {
        return allocationsRepository.findById(id);
    }

    @Override
    public void delete(Allocations allocations) {
        allocationsRepository.delete(allocations);
    }

    public Allocations updateEntityFromConsolidation(Allocations allocations, Long consolidationId) throws RunnerException {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (allocations.getId() != null) {
                long id = allocations.getId();
                Optional<Allocations> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Allocation is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            allocations = save(allocations);
            return allocations;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public Allocations updateEntityFromShipmentConsole(Allocations allocations) throws RunnerException {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (allocations.getId() != null) {
                long id = allocations.getId();
                Optional<Allocations> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Allocations is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            allocations = save(allocations);
            return allocations;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
