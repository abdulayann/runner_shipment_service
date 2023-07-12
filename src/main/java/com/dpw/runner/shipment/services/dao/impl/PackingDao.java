package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
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
public class PackingDao implements IPackingDao {
    @Autowired
    private IPackingRepository packingRepository;

    @Override
    public Packing save(Packing packing) {
        return packingRepository.save(packing);
    }

    @Override
    public Page<Packing> findAll(Specification<Packing> spec, Pageable pageable) {
        return packingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Packing> findById(Long id) {
        return packingRepository.findById(id);
    }

    @Override
    public void delete(Packing packing) {
        packingRepository.delete(packing);
    }

    public List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Packing> hashMap = packings.stream()
                    .collect(Collectors.toMap(Packing::getId, Function.identity()));
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = savePackings(packingRequestList);
            }
            deletePackings(hashMap);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private List<Packing> savePackings(List<Packing> packings) {
        List<Packing> res = new ArrayList<>();
        for(Packing req : packings){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deletePackings(Map<Long, Packing> hashMap) {
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
