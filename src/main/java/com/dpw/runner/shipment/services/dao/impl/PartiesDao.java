package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class PartiesDao implements IPartiesDao {
    @Autowired
    private IPartiesRepository partiesRepository;

    @Override
    public List<Parties> saveAll(List<Parties> parties) {
        return partiesRepository.saveAll(parties);
    }

    @Override
    public Parties save(Parties parties) {
        return partiesRepository.save(parties);
    }

    @Override
    public Page<Parties> findAll(Specification<Parties> spec, Pageable pageable) {
        return partiesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Parties> findById(Long id) {
        return partiesRepository.findById(id);
    }

    @Override
    public void delete(Parties parties) {
        partiesRepository.delete(parties);
    }

    public Parties updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            PartiesRequest partiesRequest = (PartiesRequest) commonRequestModel.getData();
            if (partiesRequest.getId() != null) {
                long id = partiesRequest.getId();
                Optional<Parties> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Parties is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            Parties parties = CommonUtils.convertToClass(partiesRequest, Parties.class);
            parties = save(parties);
            return parties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
