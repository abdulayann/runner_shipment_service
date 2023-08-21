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

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

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
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromShipment(packingRequestList, shipmentId);
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

    public List<Packing> updateEntityFromBooking(List<Packing> packingList, Long bookingId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Packing> hashMap = packings.stream()
                    .collect(Collectors.toMap(Packing::getId, Function.identity()));
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromBooking(packingRequestList, bookingId);
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

    @Override
    public List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Packing> hashMap = packings.stream()
                    .collect(Collectors.toMap(Packing::getId, Function.identity()));
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromConsole(packingRequestList, consolidationId);
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

    @Override
    public List<Packing> getAllPackings() {
        return packingRepository.findAll();
    }

    @Override
    public List<Packing> saveAll(List<Packing> packingList) {
        return packingRepository.saveAll(packingList);
    }

    public List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookingId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setBookingId(bookingId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    @Override
    public List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setConsolidationId(consolidationId);
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

    public List<Packing> savePacks(List<Packing> packs, Long contianerId)
    {
        List<Packing> res = new ArrayList<>();
        for(Packing req : packs){
            if(req.getId() != null){
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Container is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setContainerId(contianerId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Packing> removeContainerFromPacking(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            removeEntityFromContainer(packings.getContent(), null, updatedPacksId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Packing> insertContainerInPacking(List<Packing> packings, Long containerId) throws Exception {
        List<Packing> res = new ArrayList<>();
        Optional<Packing> oldEntity = Optional.empty();
        for(Packing req : packings){
                if(req.getId() != null){
                    long id = req.getId();
                    oldEntity = findById(id);
                    if (!oldEntity.isPresent()) {
                        log.debug("Packing is null for Id {}", req.getId());
                        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                    }
                }
                if(oldEntity.isPresent() && oldEntity.get().getContainerId() != containerId) {
                    req = oldEntity.get();
                    req.setContainerId(containerId);
                    req = save(req);
                    res.add(req);
                }
            }
        return res;
    }

    public List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId) {
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
            req.setContainerId(containerId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Packing> removeEntityFromContainer(List<Packing> packings, Long containerId, List<Long> updatedPacksId) {
        List<Packing> res = new ArrayList<>();
        HashSet<Long> remaniningPacksId = new HashSet<>();
        for(Long packId : updatedPacksId) {
            remaniningPacksId.add(packId);
        }
        for(Packing req : packings){
            if(!remaniningPacksId.contains(req.getId())) {
                if(req.getId() != null){
                    long id = req.getId();
                    Optional<Packing> oldEntity = findById(id);
                    if (!oldEntity.isPresent()) {
                        log.debug("Packing is null for Id {}", req.getId());
                        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                    }
                }
                req.setContainerId(null);
                req = save(req);
                res.add(req);
            }
        }
        return res;
    }

    public void deleteEntityFromContainer(Long containerId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
        saveEntityFromContainer(packings.getContent(), null);
    }

    public List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Packing> oldEntityList) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        Map<UUID, Packing> packingMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Packing entity:
                    oldEntityList) {
                packingMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Packing oldEntity;
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    oldEntity = packingMap.get(request.getGuid());
                    if(oldEntity != null) {
                        packingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromShipment(packingRequestList, shipmentId);
            }
            Map<Long, Packing> hashMap = new HashMap<>();
            packingMap.forEach((s, packing) ->  hashMap.put(packing.getId(), packing));

            deletePackings(hashMap);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
