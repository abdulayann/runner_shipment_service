package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
@Slf4j
public class AdditionalDetailDao implements IAdditionalDetailDao {
    @Autowired
    private IAdditionalDetailRepository additionalDetailRepository;

    @Override
    public AdditionalDetails save(AdditionalDetails additionalDetails) {
        return additionalDetailRepository.save(additionalDetails);
    }

    @Override
    public Optional<AdditionalDetails> findById(Long id) {
        return additionalDetailRepository.findById(id);
    }

    public AdditionalDetails updateEntityFromShipment(AdditionalDetails additionalDetail) throws RunnerException {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (additionalDetail.getId() != null) {
                long id = additionalDetail.getId();
                Optional<AdditionalDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("AdditionalDetails is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            additionalDetail = save(additionalDetail);
            return additionalDetail;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<AdditionalDetails> findByIds(List<Long> id) {
        return additionalDetailRepository.findByIdIn(id);
    }

}
