package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.Parties;
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

import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

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
        Set<String> errors = applyCarrierBookingValidations(carrierBooking);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return carrierBookingRepository.save(carrierBooking);
    }

    public List<Long> getCarrierBookingIdsListFromBol(String bol) {
        var currentTenant = TenantContext.getCurrentTenant();
        return carrierBookingRepository.findByBol(bol, currentTenant);
    }

    public List<Long> getCarrierBookingIdsListFromForwarderRefNumber(String forwarderRefNumber) {
        return carrierBookingRepository.findByForwarderRefNumber(forwarderRefNumber, TenantContext.getCurrentTenant());
    }


    public Set<String> applyCarrierBookingValidations(CarrierBooking request) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(request) , Constants.CARRIER_BOOKING, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (request.getId() != null) {
            Optional<CarrierBooking> oldEntity = findById(request.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Carrier Booking is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            // Validate lock status
            validateLockStatus(oldEntity.get());
        }
        // Duplicate party types not allowed
        validateConsolidationAddresses(request, errors);

        // MBL number must be unique
        validateMBL(request, errors);

        // Forward Reference No can not be repeated
        validateForwardRefNumber(request, errors);

        // Validate lock status
        validateLockStatus(request);

        return errors;
    }

    private void validateLockStatus(CarrierBooking carrierBooking) {
        if(carrierBooking.getIsLocked() != null && carrierBooking.getIsLocked())
            throw new ValidationException(CarrierBookingConstants.CARRIER_BOOKING_LOCKED);
    }

    private void validateForwardRefNumber(CarrierBooking request, Set<String> errors) {
        if(!IsStringNullOrEmpty(request.getForwarderRefNumber())) {
            List<Long> carrierBookingsList = getCarrierBookingIdsListFromForwarderRefNumber(request.getForwarderRefNumber());
            if(!carrierBookingsList.isEmpty() && (request.getId() == null ||
                    carrierBookingsList.stream().anyMatch(carrierBookingId -> !carrierBookingId.equals(request.getId())))) {
                errors.add("Carrier Booking with ForwardReferenceNo " + request.getForwarderRefNumber() + " already exists.");
            }
        }
    }

    private void validateMBL(CarrierBooking request, Set<String> errors) {
        if(!IsStringNullOrEmpty(request.getBol())) {
            List<Long> carrierBookingIdsList = getCarrierBookingIdsListFromBol(request.getBol());
            if(checkSameMblExists(carrierBookingIdsList, request))
                errors.add(String.format("The MBL Number %s is already used. Please use a different MBL Number", request.getBol()));
        }
    }

    public boolean checkSameMblExists(List<Long> carrierBookingIdsList, CarrierBooking request) {
        if(carrierBookingIdsList == null)
            return false;
        if(carrierBookingIdsList.isEmpty())
            return false;
        if(request.getId() == null)
            return true;
        if(carrierBookingIdsList.size() > 1)
            return true;
        return request.getId().longValue() != carrierBookingIdsList.get(0).longValue();
    }

    private void validateConsolidationAddresses(CarrierBooking request, Set<String> errors) {
        if (request.getConsolidationAddresses() != null && !request.getConsolidationAddresses().isEmpty()) {
            HashSet<String> partyTypes = new HashSet<>();
            HashSet<String> duplicatePartyTypes = new HashSet<>();
            for (Parties item : request.getConsolidationAddresses()) {
                if (partyTypes.contains(item.getType())) {
                    duplicatePartyTypes.add(item.getType());
                } else {
                    partyTypes.add(item.getType());
                }
            }
            if (!duplicatePartyTypes.isEmpty()) {
                String types = String.join(", ", duplicatePartyTypes);
                String message = (duplicatePartyTypes.size() == 1) ? " is a duplicate Party Type." : " are duplicate Party Types.";
                errors.add(types + message);
            }
        }
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
    public Optional<CarrierBooking> findByGuid(UUID id) {
        return carrierBookingRepository.findByGuid(id);
    }

    @Override
    public void delete(CarrierBooking carrierBooking) {
        carrierBookingRepository.delete(carrierBooking);
    }

    @Override
    public Optional<Long> getMaxId() {
        return carrierBookingRepository.findMaxId();
    }

    public boolean existsByIntraBookingId(String intraBookingId) {
        return carrierBookingRepository.existsByIntraBookingId(intraBookingId);
    }

    public List<CarrierBooking> saveAll(List<CarrierBooking> carrierBookingList) {
        for(var carrierBooking : carrierBookingList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(carrierBooking), Constants.CARRIER_BOOKING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return carrierBookingRepository.saveAll(carrierBookingList);
    }


}
