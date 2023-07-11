package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

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

    public List<ReferenceNumbers> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
            Page<ReferenceNumbers> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ReferenceNumbers> hashMap = routings.stream()
                    .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));
            List<ReferenceNumbersRequest> referenceNumbersRequests = new ArrayList<>();
            List<ReferenceNumbersRequest> requestList = (List<ReferenceNumbersRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (ReferenceNumbersRequest request : requestList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveReferenceNumbers(referenceNumbersRequests);
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

    private List<ReferenceNumbers> saveReferenceNumbers(List<ReferenceNumbersRequest> referenceNumbersRequests) {
        List<ReferenceNumbers> res = new ArrayList<>();
        for(ReferenceNumbersRequest req : referenceNumbersRequests){
            ReferenceNumbers saveEntity = convertToClass(req, ReferenceNumbers.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<ReferenceNumbers> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Reference number is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
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
