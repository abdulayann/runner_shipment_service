package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
    public void delete(PickupDeliveryDetails pickupDeliveryDetails) {
        pickupDeliveryDetailsRepository.delete(pickupDeliveryDetails);
    }

    public List<PickupDeliveryDetails> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<PickupDeliveryDetails> responsePickupDeliveryDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<PickupDeliveryDetails>, Pageable> pair = fetchData(listCommonRequest, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage = findAll(pair.getLeft(), pair.getRight());
            Map<Long, PickupDeliveryDetails> hashMap = pickupDeliveryDetailsPage.stream()
                    .collect(Collectors.toMap(PickupDeliveryDetails::getId, Function.identity()));
            List<PickupDeliveryDetailsRequest> serviceDetailsRequests = new ArrayList<>();
            List<PickupDeliveryDetailsRequest> requestList = (List<PickupDeliveryDetailsRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (PickupDeliveryDetailsRequest request : requestList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    serviceDetailsRequests.add(request);
                }
                responsePickupDeliveryDetails = savePickupDeliveryDetails(serviceDetailsRequests);
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

    private List<PickupDeliveryDetails> savePickupDeliveryDetails(List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequests) {
        List<PickupDeliveryDetails> res = new ArrayList<>();
        for(PickupDeliveryDetailsRequest req : pickupDeliveryDetailsRequests){
            PickupDeliveryDetails saveEntity = convertToClass(req, PickupDeliveryDetails.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<PickupDeliveryDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("PickupDeliveryDetails is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
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
