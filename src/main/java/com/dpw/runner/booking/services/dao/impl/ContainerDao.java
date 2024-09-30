package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.commons.constants.DaoConstants;
import com.dpw.runner.booking.services.commons.enums.DBOperationType;
import com.dpw.runner.booking.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.booking.services.commons.requests.ListCommonRequest;
import com.dpw.runner.booking.services.dao.interfaces.IContainerDao;
import com.dpw.runner.booking.services.dao.interfaces.IPackingDao;
import com.dpw.runner.booking.services.entity.Containers;
import com.dpw.runner.booking.services.entity.CustomerBooking;
import com.dpw.runner.booking.services.entity.Events;
import com.dpw.runner.booking.services.entity.enums.LifecycleHooks;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import com.dpw.runner.booking.services.exception.exceptions.ValidationException;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.helpers.LoggerHelper;
import com.dpw.runner.booking.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.booking.services.service.interfaces.IAuditLogService;
import com.dpw.runner.booking.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.booking.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.booking.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.booking.services.utils.CommonUtils.constructListCommonRequest;


@Repository
@Slf4j
public class ContainerDao implements IContainerDao {
    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IPackingDao packingDao;

    @Override
    public Containers save(Containers containers) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(containers) , Constants.CONTAINER, LifecycleHooks.ON_CREATE, false);
        if(Boolean.TRUE.equals(containers.getHazardous()) && IsStringNullOrEmpty(containers.getDgClass())) {
            errors.add("DG class is mandatory for Hazardous Goods Containers");
        }
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if(containers.getId() != null) {
            long id = containers.getId();
            Optional<Containers> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            containers.setCreatedAt(oldEntity.get().getCreatedAt());
            containers.setCreatedBy(oldEntity.get().getCreatedBy());
            if(containers.getEventsList() == null) {
                containers.setEventsList(oldEntity.get().getEventsList());
            }
        }
        if(containers.getEventsList() != null && containers.getEventsList().size() > 0) {
            for (Events events : containers.getEventsList()) {
                events.setEntityType(Constants.CONTAINER);
            }
        }
        containers.setIsAttached(false);
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

    private void deleteByIds(List<Long> ids) {
        if(ids != null && ids.size() > 0) {
            for (Long id: ids)
                deleteById(id);
        }
    }
    @Override
    public void deleteById(Long id) {
        containerRepository.deleteById(id);
    }

    public List<Containers> updateEntityFromBooking(List<Containers> containersList, Long bookingId) throws RunnerException {
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
            deleteContainers(hashMap, "CustomerBooking", bookingId);
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public List<Containers> saveEntityFromBooking(List<Containers> containers, Long bookingId) {
        List<Containers> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> containersPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Containers> hashMap = containersPage.stream()
                .collect(Collectors.toMap(Containers::getId, Function.identity()));
        for (Containers req : containers) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug("Containers is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                operation = DBOperationType.UPDATE.name();
            }
            req.setBookingId(bookingId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Containers.class) : null)
                                .parent(CustomerBooking.class.getSimpleName())
                                .parentId(bookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }

    private void deleteContainers(Map<Long, Containers> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(container -> {
                String json = jsonHelper.convertToJson(container);
                delete(container);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Containers.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
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

    @Override
    public List<Containers> findByShipmentId(Long shipmentId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentsList", shipmentId, "CONTAINS");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> containersPage = findAll(pair.getLeft(), pair.getRight());
        return containersPage.getContent();
    }

    @Override
    public List<Containers> findByConsolidationId(Long consolidationId) {
        return containerRepository.findByConsolidationId(consolidationId);
    }

}
