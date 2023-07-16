package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
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
public class RoutingsDao implements IRoutingsDao {
    @Autowired
    private IRoutingsRepository routingsRepository;

    @Override
    public Routings save(Routings routings) {
        return routingsRepository.save(routings);
    }

    @Override
    public Page<Routings> findAll(Specification<Routings> spec, Pageable pageable) {
        return routingsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Routings> findById(Long id) {
        return routingsRepository.findById(id);
    }

    @Override
    public void delete(Routings routings) {
        routingsRepository.delete(routings);
    }

    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
            Page<Routings> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Routings> hashMap = routings.stream()
                    .collect(Collectors.toMap(Routings::getId, Function.identity()));
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveRoutings(routingsRequestList, shipmentId);
            }
            deleteRoutings(hashMap);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Routings> saveRoutings(List<Routings> routings, Long shipmentId) {
        List<Routings> res = new ArrayList<>();
        for(Routings req : routings){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Routings> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteRoutings(Map<Long, Routings> hashMap) {
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
