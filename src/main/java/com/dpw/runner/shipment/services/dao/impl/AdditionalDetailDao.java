package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.entity.AdditionalDetail;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailRepository;
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
public class AdditionalDetailDao implements IAdditionalDetailDao {
    @Autowired
    private IAdditionalDetailRepository additionalDetailRepository;

    @Override
    public AdditionalDetail save(AdditionalDetail additionalDetail) {
        return additionalDetailRepository.save(additionalDetail);
    }

    @Override
    public Page<AdditionalDetail> findAll(Specification<AdditionalDetail> spec, Pageable pageable) {
        return additionalDetailRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<AdditionalDetail> findById(Long id) {
        return additionalDetailRepository.findById(id);
    }

    @Override
    public void delete(AdditionalDetail additionalDetail) {
        additionalDetailRepository.delete(additionalDetail);
    }

    public AdditionalDetail updateEntityFromShipment(AdditionalDetail additionalDetail, Long shipmentId) throws Exception
    {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (additionalDetail.getId() != null) {
                long id = additionalDetail.getId();
                Optional<AdditionalDetail> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("AdditionalDetail is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            additionalDetail = save(additionalDetail);
            return additionalDetail;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
