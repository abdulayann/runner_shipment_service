package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsRepository;
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
public class ELDetailsDao implements IELDetailsDao {
    @Autowired
    private IELDetailsRepository elDetailsRepository;

    @Override
    public ELDetails save(ELDetails elDetails) {
        return elDetailsRepository.save(elDetails);
    }

    @Override
    public Page<ELDetails> findAll(Specification<ELDetails> spec, Pageable pageable) {
        return elDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ELDetails> findById(Long id) {
        return elDetailsRepository.findById(id);
    }

    @Override
    public void delete(ELDetails elDetails) {
        elDetailsRepository.delete(elDetails);
    }

    @Override
    public Optional<ELDetails> findByElNumber(String elNumber) {
        return elDetailsRepository.findByElNumber(elNumber);
    }

    public List<ELDetails> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<ELDetails> responseELDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ELDetails>, Pageable> pair = fetchData(listCommonRequest, ELDetails.class);
            Page<ELDetails> elDetails = findAll(pair.getLeft(), pair.getRight());
            Map<Long, ELDetails> hashMap = elDetails.stream()
                    .collect(Collectors.toMap(ELDetails::getId, Function.identity()));
            List<ELDetailsRequest> elDetailsRequestList = new ArrayList<>();
            List<ELDetailsRequest> requestList = (List<ELDetailsRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (ELDetailsRequest request : requestList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    elDetailsRequestList.add(request);
                }
                responseELDetails = saveELDetails(elDetailsRequestList);
            }
            deleteELDetails(hashMap);
            return responseELDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private List<ELDetails> saveELDetails(List<ELDetailsRequest> elDetails) {
        List<ELDetails> res = new ArrayList<>();
        for(ELDetailsRequest req : elDetails){
            ELDetails saveEntity = convertToClass(req, ELDetails.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<ELDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("EL Detail is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
        }
        return res;
    }

    private void deleteELDetails(Map<Long, ELDetails> hashMap) {
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
