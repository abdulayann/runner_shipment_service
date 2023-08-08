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

    @Override
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
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId);
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

    @Override
    public List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId) {
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

    public List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
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
                responseRoutings = saveEntityFromConsole(routingsRequestList, consolidationId);
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

    @Override
    public List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId) {
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
            req.setConsolidationId(consolidationId);
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

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, Routings> routingMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Routings entity:
                    oldEntityList) {
                routingMap.put(entity.getGuid(), entity);
            }
        }

        List<Routings> responseRoutings = new ArrayList<>();
        try {
            Routings oldEntity;
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    oldEntity = routingMap.get(request.getGuid());
                    if(oldEntity != null) {
                        routingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId);
            }
            Map<Long, Routings> hashMap = new HashMap<>();
            routingMap.forEach((s, routings) ->  hashMap.put(routings.getId(), routings));
            deleteRoutings(hashMap);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
