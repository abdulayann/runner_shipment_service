package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ICarrierBookingRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
@Slf4j
public class CarrierBookingDao implements ICarrierBookingDao {

    @Autowired
    private ICarrierBookingRepository carrierBookingRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;


    @Override
    public CarrierBooking save(CarrierBooking carrierBooking) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(carrierBooking) , Constants.CARRIER_BOOKING, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (carrierBooking.getId() != null) {
            Optional<CarrierBooking> oldEntity = findById(carrierBooking.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Customer Booking is null for Id {}", carrierBooking.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return carrierBookingRepository.save(carrierBooking);
    }

    @Override
    public Page<CarrierBooking> findAll(Specification<CarrierBooking> spec, Pageable pageable) {
        return carrierBookingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<CarrierBooking> findById(Long id) {
        return carrierBookingRepository.findById(id);
    }

    @Override
    public void delete(CarrierBooking carrierBooking) {
        carrierBookingRepository.delete(carrierBooking);
    }

}
