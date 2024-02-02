package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsRepository;
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
public class TruckDriverDetailsDao implements ITruckDriverDetailsDao {
    @Autowired
    private ITruckDriverDetailsRepository truckDriverDetailsRepository;

    @Override
    public TruckDriverDetails save(TruckDriverDetails truckDriverDetails) {
        return truckDriverDetailsRepository.save(truckDriverDetails);
    }
    @Override
    public List<TruckDriverDetails> saveAll(List<TruckDriverDetails> truckDriverDetailsList) {
        return truckDriverDetailsRepository.saveAll(truckDriverDetailsList);
    }

    @Override
    public Page<TruckDriverDetails> findAll(Specification<TruckDriverDetails> spec, Pageable pageable) {
        return truckDriverDetailsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<TruckDriverDetails> findById(Long id) {
        return truckDriverDetailsRepository.findById(id);
    }

    @Override
    public void delete(TruckDriverDetails truckDriverDetails) {
        truckDriverDetailsRepository.delete(truckDriverDetails);
    }

    private void deleteTruckDriverDetails(Map<Long, TruckDriverDetails> hashMap) {
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
    public List<TruckDriverDetails> updateEntityFromShipment(List<TruckDriverDetails> truckDriverDetailsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<TruckDriverDetails> responseTruckDriverDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, TruckDriverDetails> hashMap = new HashMap<>();
            var truckDriverDetailsIdList = truckDriverDetailsList.stream().map(TruckDriverDetails::getId).toList();
            if(!Objects.isNull(truckDriverDetailsIdList) && !truckDriverDetailsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
                Pair<Specification<TruckDriverDetails>, Pageable> pair = fetchData(listCommonRequest, TruckDriverDetails.class);
                Page<TruckDriverDetails> truckDriverDetails = findAll(pair.getLeft(), pair.getRight());
                hashMap = truckDriverDetails.stream()
                        .collect(Collectors.toMap(TruckDriverDetails::getId, Function.identity()));
            }
            Map<Long, TruckDriverDetails> copyHashMap = new HashMap<>(hashMap);
            List<TruckDriverDetails> truckDriverDetailsRequestList = new ArrayList<>();
            if (truckDriverDetailsList != null && truckDriverDetailsList.size() != 0) {
                for (TruckDriverDetails request : truckDriverDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    truckDriverDetailsRequestList.add(request);
                }
                responseTruckDriverDetails = saveEntityFromShipment(truckDriverDetailsRequestList, shipmentId, copyHashMap);
            }
            deleteTruckDriverDetails(hashMap);
            return responseTruckDriverDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<TruckDriverDetails> saveEntityFromShipment(List<TruckDriverDetails> truckDriverDetails, Long shipmentId) {
        List<TruckDriverDetails> res = new ArrayList<>();
        for(TruckDriverDetails req : truckDriverDetails){
            if(req.getId() != null){
                long id = req.getId();
                Optional<TruckDriverDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Truck driver detail is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<TruckDriverDetails> saveEntityFromShipment(List<TruckDriverDetails> truckDriverDetails, Long shipmentId, Map<Long, TruckDriverDetails> oldEntityMap) {
        List<TruckDriverDetails> res = new ArrayList<>();
        for(TruckDriverDetails req : truckDriverDetails){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Truck driver detail is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }

    @Override
    public List<TruckDriverDetails> updateEntityFromShipment(List<TruckDriverDetails> truckDriverDetailsList, Long shipmentId, List<TruckDriverDetails> oldEntityList) throws Exception {
        String responseMsg;
        List<TruckDriverDetails> responseTruckDriverDetails = new ArrayList<>();
        Map<UUID, TruckDriverDetails> truckDriverDetailsMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (TruckDriverDetails entity:
                    oldEntityList) {
                truckDriverDetailsMap.put(entity.getGuid(), entity);
            }
        }
        try {

            TruckDriverDetails oldEntity;
            List<TruckDriverDetails> truckDriverDetailsRequestList = new ArrayList<>();
            if (truckDriverDetailsList != null && truckDriverDetailsList.size() != 0) {
                for (TruckDriverDetails request : truckDriverDetailsList) {
                    oldEntity = truckDriverDetailsMap.get(request.getGuid());
                    if(oldEntity != null) {
                        truckDriverDetailsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    truckDriverDetailsRequestList.add(request);
                }
                responseTruckDriverDetails = saveEntityFromShipment(truckDriverDetailsRequestList, shipmentId);
            }
            Map<Long, TruckDriverDetails> hashMap = new HashMap<>();
            truckDriverDetailsMap.forEach((s, truckDriverDetail) ->  hashMap.put(truckDriverDetail.getId(), truckDriverDetail));

            deleteTruckDriverDetails(hashMap);
            return responseTruckDriverDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<TruckDriverDetails> updateEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsList, Long consolidationId) throws Exception {
        String responseMsg;
        List<TruckDriverDetails> responseTruckDriverDetails = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, TruckDriverDetails> hashMap;
//            if(!Objects.isNull(truckDriverDetailsIdList) && !truckDriverDetailsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
                Pair<Specification<TruckDriverDetails>, Pageable> pair = fetchData(listCommonRequest, TruckDriverDetails.class);
                Page<TruckDriverDetails> truckDriverDetailsPage = findAll(pair.getLeft(), pair.getRight());
                hashMap = truckDriverDetailsPage.stream()
                        .collect(Collectors.toMap(TruckDriverDetails::getId, Function.identity()));
//            }
            Map<Long, TruckDriverDetails> copyHashMap = new HashMap<>(hashMap);
            List<TruckDriverDetails> truckDriverDetailsRequests = new ArrayList<>();
            if (truckDriverDetailsList != null && truckDriverDetailsList.size() != 0) {
                for (TruckDriverDetails request : truckDriverDetailsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    truckDriverDetailsRequests.add(request);
                }
                responseTruckDriverDetails = saveEntityFromConsole(truckDriverDetailsRequests, consolidationId, copyHashMap);
            }
            deleteTruckDriverDetails(hashMap);
            return responseTruckDriverDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<TruckDriverDetails> updateEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsList, Long consolidationId, List<TruckDriverDetails> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, TruckDriverDetails> truckDriverDetailsMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (TruckDriverDetails entity:
                    oldEntityList) {
                truckDriverDetailsMap.put(entity.getGuid(), entity);
            }
        }

        List<TruckDriverDetails> responseTruckDriverDetails = new ArrayList<>();
        try {
            TruckDriverDetails oldEntity;
            List<TruckDriverDetails> truckDriverDetailsRequests = new ArrayList<>();
            if (truckDriverDetailsList != null && truckDriverDetailsList.size() != 0) {
                for (TruckDriverDetails request : truckDriverDetailsList) {
                    oldEntity = truckDriverDetailsMap.get(request.getGuid());
                    if(oldEntity != null) {
                        truckDriverDetailsMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    truckDriverDetailsRequests.add(request);
                }
                responseTruckDriverDetails = saveEntityFromConsole(truckDriverDetailsRequests, consolidationId);
            }
            Map<Long, TruckDriverDetails> hashMap = new HashMap<>();
            truckDriverDetailsMap.forEach((s, truckDriverDetails) ->  hashMap.put(truckDriverDetails.getId(), truckDriverDetails));
            deleteTruckDriverDetails(hashMap);
            return responseTruckDriverDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<TruckDriverDetails> saveEntityFromConsole(List<TruckDriverDetails> truckDriverDetailsRequests, Long consolidationId) {
        List<TruckDriverDetails> res = new ArrayList<>();
        for(TruckDriverDetails req : truckDriverDetailsRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<TruckDriverDetails> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Truck driver detail is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<TruckDriverDetails> saveEntityFromConsole(List<TruckDriverDetails> truckDriverDetails, Long consolidationId, Map<Long, TruckDriverDetails> oldEntityMap) {
        List<TruckDriverDetails> res = new ArrayList<>();
        for(TruckDriverDetails req : truckDriverDetails){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Truck driver detail is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }
}
