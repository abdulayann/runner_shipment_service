package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IArrivalDepartureDetailsDao;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IArrivalDepartureDetailsRepository;
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
public class ArrivalDepartureDetailsDao implements IArrivalDepartureDetailsDao {

    @Autowired
    private IArrivalDepartureDetailsRepository arrivalDepartureDetailsRepository;

    @Override
    public ArrivalDepartureDetails save(ArrivalDepartureDetails arrivalDepartureDetails) {
        return arrivalDepartureDetailsRepository.save(arrivalDepartureDetails);
    }

    @Override
    public Page<ArrivalDepartureDetails> findAll(Specification<ArrivalDepartureDetails> spec, Pageable pageable) {
        return arrivalDepartureDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ArrivalDepartureDetails> findById(Long id) {
        return arrivalDepartureDetailsRepository.findById(id);
    }

    @Override
    public void delete(ArrivalDepartureDetails arrivalDepartureDetails) {
        arrivalDepartureDetailsRepository.delete(arrivalDepartureDetails);
    }

    @Override
    public ArrivalDepartureDetails updateEntityFromShipmentConsole(ArrivalDepartureDetails arrivalDepartureDetails) throws Exception {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (arrivalDepartureDetails.getId() != null) {
                long id = arrivalDepartureDetails.getId();
                Optional<ArrivalDepartureDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Arrival Departure Details is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            arrivalDepartureDetails = save(arrivalDepartureDetails);
            return arrivalDepartureDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
