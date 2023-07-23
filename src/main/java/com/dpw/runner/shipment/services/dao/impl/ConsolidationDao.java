package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
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

@Repository
@Slf4j
public class ConsolidationDao implements IConsolidationDetailsDao {
    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails) {
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
        consolidationRepository.delete(consolidationDetails);
    }

    public List<ConsolidationDetails> saveConsolidations(List<ConsolidationDetails> consolidationDetails)
    {
        List<ConsolidationDetails> res = new ArrayList<>();
        for(ConsolidationDetails req : consolidationDetails){
            if(req.getId() != null){
                long id = req.getId();
                Optional<ConsolidationDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Container is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req = save(req);
            res.add(req);
        }
        return res;
    }
}
