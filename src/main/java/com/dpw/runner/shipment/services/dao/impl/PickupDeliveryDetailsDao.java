package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

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
    public Optional<PickupDeliveryDetails> findByGuid(UUID guid) {
        return pickupDeliveryDetailsRepository.findByGuid(guid);
    }

    @Override
    public void delete(PickupDeliveryDetails pickupDeliveryDetails) {
        pickupDeliveryDetailsRepository.delete(pickupDeliveryDetails);
    }

    public List<PickupDeliveryDetails> updateEntityFromShipment(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<PickupDeliveryDetails> responsePickupDeliveryDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<PickupDeliveryDetails>, Pageable> pair = fetchData(listCommonRequest, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage = findAll(pair.getLeft(), pair.getRight());
            Map<Long, PickupDeliveryDetails> hashMap = pickupDeliveryDetailsPage.stream()
                    .collect(Collectors.toMap(PickupDeliveryDetails::getId, Function.identity()));
            List<PickupDeliveryDetails> pickupDeliveryDetails = new ArrayList<>();
            if (pickupDeliveryDetailsList != null && pickupDeliveryDetailsList.size() != 0) {
                for (PickupDeliveryDetails request : pickupDeliveryDetailsList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    pickupDeliveryDetails.add(request);
                }
                responsePickupDeliveryDetails = savePickupDeliveryDetails(pickupDeliveryDetails);
            }
            deletePickupDeliveryDetails(hashMap);
            return responsePickupDeliveryDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private List<PickupDeliveryDetails> savePickupDeliveryDetails(List<PickupDeliveryDetails> pickupDeliveryDetailsRequests) {
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
}
