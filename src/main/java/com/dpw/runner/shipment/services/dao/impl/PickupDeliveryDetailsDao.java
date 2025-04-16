package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

@Repository
@Slf4j
public class PickupDeliveryDetailsDao implements IPickupDeliveryDetailsDao {
    @Autowired
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;

    @Override
    public PickupDeliveryDetails save(PickupDeliveryDetails pickupDeliveryDetails) {
        return pickupDeliveryDetailsRepository.save(pickupDeliveryDetails);
    }

    @Override
    public Page<PickupDeliveryDetails> findAll(Specification<PickupDeliveryDetails> spec, Pageable pageable) {
        return pickupDeliveryDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<PickupDeliveryDetails> findById(Long id) {
        return pickupDeliveryDetailsRepository.findById(id);
    }

    @Override
    public void delete(PickupDeliveryDetails pickupDeliveryDetails) {
        pickupDeliveryDetailsRepository.delete(pickupDeliveryDetails);
    }

    public List<PickupDeliveryDetails> updateEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<PickupDeliveryDetails> responsePickupDeliveryDetails = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            List<PickupDeliveryDetails> pickupDeliveryDetailsPage = findByShipmentId(shipmentId);
            Map<Long, PickupDeliveryDetails> hashMap = pickupDeliveryDetailsPage.stream()
                    .collect(Collectors.toMap(PickupDeliveryDetails::getId, Function.identity()));
            List<PickupDeliveryDetails> pickupDeliveryDetails = new ArrayList<>();
            if (pickupDeliveryDetailsList != null && !pickupDeliveryDetailsList.isEmpty()) {
                for (PickupDeliveryDetails request : pickupDeliveryDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    pickupDeliveryDetails.add(request);
                }
                responsePickupDeliveryDetails = saveEntityFromShipment(pickupDeliveryDetails, shipmentId);
            }
            deletePickupDeliveryDetails(hashMap);
            return responsePickupDeliveryDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<PickupDeliveryDetails> findByShipmentId(Long shipmentId) {
        return pickupDeliveryDetailsRepository.findByShipmentId(shipmentId);
    }

    public List<PickupDeliveryDetails> saveEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsRequests, Long shipmentId) {
        List<PickupDeliveryDetails> res = new ArrayList<>();
        for(PickupDeliveryDetails req : pickupDeliveryDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<PickupDeliveryDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("PickupDeliveryDetails is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deletePickupDeliveryDetails(Map<Long, PickupDeliveryDetails> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<PickupDeliveryDetails> findByIdIn(List<Long> ids) {
        return pickupDeliveryDetailsRepository.findByIdIn(ids);
    }
}
