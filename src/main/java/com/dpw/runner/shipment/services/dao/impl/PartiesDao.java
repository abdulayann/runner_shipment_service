package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListRequestFromEntityId;

@Repository
@Slf4j
public class PartiesDao implements IPartiesDao {
    @Autowired
    private IPartiesRepository partiesRepository;

    @Override
    public List<Parties> saveAll(List<Parties> parties) {
        return partiesRepository.saveAll(parties);
    }

    @Override
    public Parties save(Parties parties) {return partiesRepository.save(parties);}

    @Override
    public Page<Parties> findAll(Specification<Parties> spec, Pageable pageable) {
        return partiesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Parties> findById(Long id) {
        return partiesRepository.findById(id);
    }

    @Override
    public void delete(Parties parties) {
        partiesRepository.delete(parties);
    }

    public Parties updateEntityFromShipment(Parties parties, Long shipmentId) throws Exception {
        String responseMsg;
        try {
            // TODO- Handle Transactions here
            if (parties.getId() != null) {
                long id = parties.getId();
                Optional<Parties> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Parties is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            parties = save(parties);
            return parties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType) throws Exception {
        String responseMsg;
        List<Parties> responseParties = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entityId, entityType);
            Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
            Page<Parties> parties = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Parties> hashMap = parties.stream()
                    .collect(Collectors.toMap(Parties::getId, Function.identity()));
            List<Parties> partiesRequestList = new ArrayList<>();
            if (partiesList != null && partiesList.size() != 0) {
                for (Parties request : partiesList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    partiesRequestList.add(request);
                }
                responseParties = saveEntityFromOtherEntity(partiesRequestList, entityId, entityType);
            }
            deleteParties(hashMap);
            return responseParties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Parties> saveEntityFromOtherEntity(List<Parties> partiesRequests, Long entityId, String entityType) {
        List<Parties> res = new ArrayList<>();
        for(Parties req : partiesRequests){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Parties> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Parties is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setEntityId(entityId);
            req.setEntityType(entityType);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteParties(Map<Long, Parties> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<Parties> updateEntityFromOtherEntity(List<Parties> partiesList, Long entityId, String entityType, List<Parties> oldEntityList) throws Exception {
        String responseMsg;
        List<Parties> responseParties = new ArrayList<>();
        Map<UUID, Parties> partiesMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Parties entity:
                    oldEntityList) {
                partiesMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Parties oldEntity;
            List<Parties> partiesRequestList = new ArrayList<>();
            if (partiesList != null && partiesList.size() != 0) {
                for (Parties request : partiesList) {
                    oldEntity = partiesMap.get(request.getGuid());
                    if(oldEntity != null) {
                        partiesMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    request.setEntityId(entityId);
                    request.setEntityType(entityType);
                    partiesRequestList.add(request);
                }
                responseParties = saveEntityFromOtherEntity(partiesRequestList, entityId, entityType);
            }
            Map<Long, Parties> hashMap = new HashMap<>();
            partiesMap.forEach((s, parties) ->  hashMap.put(parties.getId(), parties));
            deleteParties(hashMap);
            return responseParties;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

}
