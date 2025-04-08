package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
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

@Repository
@Slf4j
public class BookingCarriageDao implements IBookingCarriageDao {
    @Autowired
    private IBookingCarriageRepository bookingCarriageRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public BookingCarriage save(BookingCarriage bookingCarriage) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingCarriage) , Constants.CARRIAGE, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return bookingCarriageRepository.save(bookingCarriage);
    }
    @Override
    public List<BookingCarriage> saveAll(List<BookingCarriage> bookingCarriageList) {
        for(var bookingCarriage: bookingCarriageList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingCarriage), Constants.CARRIAGE, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return bookingCarriageRepository.saveAll(bookingCarriageList);
    }

    @Override
    public Page<BookingCarriage> findAll(Specification<BookingCarriage> spec, Pageable pageable) {
        return bookingCarriageRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingCarriage> findById(Long id) {
        return bookingCarriageRepository.findById(id);
    }

    @Override
    public void delete(BookingCarriage bookingCarriage) {
        bookingCarriageRepository.delete(bookingCarriage);
    }

    public List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<BookingCarriage> responseBookingCarriage = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            List<BookingCarriage> bookingCarriages = findByShipmentId(shipmentId);
            Map<Long, BookingCarriage> hashMap = bookingCarriages.stream()
                        .collect(Collectors.toMap(BookingCarriage::getId, Function.identity()));
            Map<Long, BookingCarriage> copyHashMap = new HashMap<>(hashMap);
            List<BookingCarriage> bookingCarriagesRequestList = new ArrayList<>();
            if (bookingCarriageList != null && !bookingCarriageList.isEmpty()) {
                for (BookingCarriage request : bookingCarriageList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingCarriagesRequestList.add(request);
                }
                responseBookingCarriage = saveEntityFromShipment(bookingCarriagesRequestList, shipmentId, copyHashMap);
            }
            deleteBookingCarriage(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseBookingCarriage;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<BookingCarriage> findByShipmentId(Long shipmentId) {
        return bookingCarriageRepository.findByShipmentId(shipmentId);
    }

    public List<BookingCarriage> saveEntityFromShipment(List<BookingCarriage> bookingCarriages, Long shipmentId) {
        List<BookingCarriage> res = new ArrayList<>();
        for(BookingCarriage req : bookingCarriages){
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(req.getId() != null){
                long id = req.getId();
                Optional<BookingCarriage> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, BookingCarriage.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
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
    @Override
    public List<BookingCarriage> saveEntityFromShipment(List<BookingCarriage> bookingCarriages, Long shipmentId, Map<Long, BookingCarriage> oldEntityMap) {
        List<BookingCarriage> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for(BookingCarriage req : bookingCarriages){
            if(req.getId() != null){
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, BookingCarriage.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    void deleteBookingCarriage(Map<Long, BookingCarriage> hashMap, String entityType, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(bookingCarriage -> {
                String json = jsonHelper.convertToJson(bookingCarriage);
                delete(bookingCarriage);
                if(entityType != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, BookingCarriage.class))
                                        .parent(entityType)
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

    public List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId, List<BookingCarriage> oldEntityList) throws RunnerException {
        String responseMsg;
        List<BookingCarriage> responseBookingCarriage = new ArrayList<>();
        Map<UUID, BookingCarriage> bookingMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (BookingCarriage entity:
                    oldEntityList) {
                bookingMap.put(entity.getGuid(), entity);
            }
        }
        try {

            BookingCarriage oldEntity;
            List<BookingCarriage> bookingCarriagesRequestList = new ArrayList<>();
            if (bookingCarriageList != null && !bookingCarriageList.isEmpty()) {
                for (BookingCarriage request : bookingCarriageList) {
                    oldEntity = bookingMap.get(request.getGuid());
                    if(oldEntity != null) {
                        bookingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    bookingCarriagesRequestList.add(request);
                }
                responseBookingCarriage = saveEntityFromShipment(bookingCarriagesRequestList, shipmentId);
            }
            Map<Long, BookingCarriage> hashMap = new HashMap<>();
            bookingMap.forEach((s, bookingCarriage) ->  hashMap.put(bookingCarriage.getId(), bookingCarriage));

            deleteBookingCarriage(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseBookingCarriage;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
