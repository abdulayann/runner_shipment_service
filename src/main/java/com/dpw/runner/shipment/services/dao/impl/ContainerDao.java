package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
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
public class ContainerDao implements IContainerDao {
    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Containers save(Containers containers) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(containers) , Constants.CONTAINER, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        if(containers.getId() != null) {
            long id = containers.getId();
            Optional<Containers> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(containers.getShipmentsList() == null) {
                containers.setShipmentsList(oldEntity.get().getShipmentsList());
            }
        }
        return containerRepository.save(containers);
    }

    @Override
    public Page<Containers> findAll(Specification<Containers> spec, Pageable pageable) {
        return containerRepository.findAll(spec, pageable);
    }

    @Override
    public List<Containers> getAllContainers() {
        return containerRepository.findAll();
    }

    @Override
    public Optional<Containers> findById(Long id) {
        return containerRepository.findById(id);
    }

    @Override
    public List<Containers> findByGuid(UUID guid) {
        return containerRepository.findByGuid(guid);
    }

    @Override
    public void delete(Containers containers) {
        containerRepository.delete(containers);
    }

    public List<Containers> updateEntityFromBooking(List<Containers> containersList, Long bookingId) throws Exception {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> hashMap = containers.stream()
                    .collect(Collectors.toMap(Containers::getId, Function.identity()));
            List<Containers> containersRequestList = new ArrayList<>();
            if (containersList != null && containersList.size() != 0) {
                for (Containers request : containersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    containersRequestList.add(request);
                }
                responseContainers = saveEntityFromBooking(containersRequestList, bookingId);
            }
            deleteContainers(hashMap);
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Containers> saveEntityFromBooking(List<Containers> containers, Long bookingId) {
        List<Containers> res = new ArrayList<>();
        for (Containers req : containers) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Containers> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Containers is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setBookingId(bookingId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    private void deleteContainers(Map<Long, Containers> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            if (containersList != null && containersList.size() != 0) {
                List<Containers> containerList = new ArrayList<>(containersList);
                if(consolidationId != null) {
                    for (Containers containers: containerList) {
                        containers.setConsolidationId(consolidationId);
                    }
                }
                responseContainers = saveAll(containerList);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Containers> saveAll(List<Containers> containers) {
        List<Containers> res = new ArrayList<>();
        for (Containers req : containers) {
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId, List<Containers> oldEntityList) throws Exception {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        Map<UUID, Containers> containersMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (Containers containers:
                 oldEntityList) {
                containersMap.put(containers.getGuid(), containers);
            }
        }
        Containers oldContainer;
        try {
            // TODO- Handle Transactions here
            if (containersList != null && containersList.size() != 0) {
                List<Containers> containerList = new ArrayList<>(containersList);
                for (Containers containers: containerList) {
                    if(containersMap.containsKey(containers.getGuid())) {
                        oldContainer = containersMap.get(containers.getGuid());
                        containers.setId(oldContainer.getId());
                        containers.setConsolidationId(oldContainer.getConsolidationId());
                    } else {
                        containers.setId(null);
                        containers.setConsolidationId(null);
                    }
                    if(consolidationId != null) {
                        containers.setConsolidationId(consolidationId);
                    }
                }
                responseContainers = saveAll(containerList);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
