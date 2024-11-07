package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IEVgmDao;
import com.dpw.runner.shipment.services.entity.BookingPayment;
import com.dpw.runner.shipment.services.entity.EVgm;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IEVgmRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
@Slf4j
public class EVgmDao implements IEVgmDao {
    @Autowired
    private IEVgmRepository eVgmRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;


    @Override
    public EVgm save(EVgm eVgm) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(eVgm) , Constants.EVGM, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (eVgm.getId() != null) {
            Optional<EVgm> oldEntity = findById(eVgm.getId());
            if (oldEntity.isEmpty()) {
                log.debug("EVGM is null for Id {}", eVgm.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return eVgmRepository.save(eVgm);
    }


    @Override
    public Page<EVgm> findAll(Specification<EVgm> spec, Pageable pageable) {
        return eVgmRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<EVgm> findById(Long id) {
        return eVgmRepository.findById(id);
    }

    @Override
    public Optional<EVgm> findByGuid(UUID id) {
        return eVgmRepository.findByGuid(id);
    }

    @Override
    public void delete(EVgm eVgm) {
        eVgmRepository.delete(eVgm);
    }

    public List<EVgm> saveAll(List<EVgm> eVgmList) {
        for(var eVgm : eVgmList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(eVgm), Constants.EVGM, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return eVgmRepository.saveAll(eVgmList);
    }

}
