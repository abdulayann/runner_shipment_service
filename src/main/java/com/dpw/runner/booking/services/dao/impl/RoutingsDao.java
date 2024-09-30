package com.dpw.runner.booking.services.dao.impl;

import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.commons.constants.DaoConstants;
import com.dpw.runner.booking.services.commons.enums.DBOperationType;
import com.dpw.runner.booking.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.booking.services.commons.requests.ListCommonRequest;
import com.dpw.runner.booking.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.booking.services.entity.CustomerBooking;
import com.dpw.runner.booking.services.entity.Routings;
import com.dpw.runner.booking.services.entity.enums.LifecycleHooks;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import com.dpw.runner.booking.services.exception.exceptions.ValidationException;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.booking.services.service.interfaces.IAuditLogService;
import com.dpw.runner.booking.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
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
import static com.dpw.runner.booking.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class RoutingsDao implements IRoutingsDao {
    public static final String ROUTING_IS_NULL_FOR_ID_MSG = "Routing is null for Id {}";
    @Autowired
    private IRoutingsRepository routingsRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public Routings save(Routings routings) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return routingsRepository.save(routings);
    }
    @Override
    public List<Routings> saveAll(List<Routings> routingsList) {
        for(var routings: routingsList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return routingsRepository.saveAll(routingsList);
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
    public Optional<Routings> findByGuid(UUID id) {
        return routingsRepository.findByGuid(id);
    }

    @Override
    public void delete(Routings routings) {
        routingsRepository.delete(routings);
    }

    @Override
    public List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
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
                responseRoutings = saveEntityFromBooking(routingsRequestList, bookingId);
            }
            deleteRoutings(hashMap, "CustomerBooking", bookingId);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId) {
        List<Routings> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
        Page<Routings> routingsPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Routings> hashMap = routingsPage.stream()
                .collect(Collectors.toMap(Routings::getId, Function.identity()));
        for (Routings req : routings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
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

    private void deleteRoutings(Map<Long, Routings> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(routing -> {
                String json = jsonHelper.convertToJson(routing);
                delete(routing);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Routings.class))
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
}
