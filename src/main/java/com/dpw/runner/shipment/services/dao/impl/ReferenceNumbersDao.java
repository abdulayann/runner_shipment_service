package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
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

@Repository
@Slf4j
public class ReferenceNumbersDao implements IReferenceNumbersDao {
    @Autowired
    private IReferenceNumbersRepository referenceNumbersRepository;

    @Override
    public ReferenceNumbers save(ReferenceNumbers referenceNumbers) {
        return referenceNumbersRepository.save(referenceNumbers);
    }

    @Override
    public Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable) {
        return referenceNumbersRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ReferenceNumbers> findById(Long id) {
        return referenceNumbersRepository.findById(id);
    }

    @Override
    public void delete(ReferenceNumbers referenceNumbers) {
        referenceNumbersRepository.delete(referenceNumbers);
    }

    public List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws Exception {
        String responseMsg;
        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
            Page<ReferenceNumbers> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ReferenceNumbers> hashMap = routings.stream()
                    .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));
            List<ReferenceNumbers> referenceNumbersRequests = new ArrayList<>();
            if (referenceNumbersList != null && referenceNumbersList.size() != 0) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveEntityFromShipment(referenceNumbersRequests, shipmentId);
            }
            deleteReferenceNumbers(hashMap);
            return responseReferenceNumbers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId) {
        List<ReferenceNumbers> res = new ArrayList<>();
        for(ReferenceNumbers req : referenceNumbersRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<ReferenceNumbers> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Reference number is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteReferenceNumbers(Map<Long, ReferenceNumbers> hashMap) {
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
