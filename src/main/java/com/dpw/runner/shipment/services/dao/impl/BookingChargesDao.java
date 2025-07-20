package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingChargesDao;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingChargesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
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

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class BookingChargesDao implements IBookingChargesDao {
    @Autowired
    private IBookingChargesRepository bookingChargesRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public BookingCharges save(BookingCharges bookingCharges) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingCharges) , Constants.BOOKING_CHARGES, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        return bookingChargesRepository.save(bookingCharges);
    }

    @Override
    public Page<BookingCharges> findAll(Specification<BookingCharges> spec, Pageable pageable) {
        return bookingChargesRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingCharges> findById(Long id) {
        return bookingChargesRepository.findById(id);
    }

    @Override
    public void delete(BookingCharges bookingCharges) {
        bookingChargesRepository.delete(bookingCharges);
    }

    public BookingCharges updateEntityFromShipmentConsole(BookingCharges bookingCharges) throws RunnerException {
        String responseMsg;
        try {
            if (bookingCharges.getId() != null) {
                long id = bookingCharges.getId();
                Optional<BookingCharges> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Charges is null for Id {}", id);
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            bookingCharges = save(bookingCharges);
            return bookingCharges;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private void deleteBookingCharges(Map<Long, BookingCharges> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(bookingCharge -> {
                String json = jsonHelper.convertToJson(bookingCharge);
                delete(bookingCharge);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, BookingCharges.class))
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

    public List<BookingCharges> saveEntityFromBooking(List<BookingCharges> bookingCharges, Long bookingId) {
        List<BookingCharges> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<BookingCharges>, Pageable> pair = fetchData(listCommonRequest, BookingCharges.class);
        Page<BookingCharges> bookingChargesPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, BookingCharges> hashMap = bookingChargesPage.stream()
                .collect(Collectors.toMap(BookingCharges::getId, Function.identity()));
        for (BookingCharges req : bookingCharges) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug("Booking Charges is null for Id {}", req.getId());
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
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, BookingCharges.class) : null)
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

    public List<BookingCharges> updateEntityFromBooking(List<BookingCharges> bookingChargesList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<BookingCharges> responseBookingCharges = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<BookingCharges>, Pageable> pair = fetchData(listCommonRequest, BookingCharges.class);
            Page<BookingCharges> bookingCharges = findAll(pair.getLeft(), pair.getRight());
            Map<Long, BookingCharges> hashMap = bookingCharges.stream()
                    .collect(Collectors.toMap(BookingCharges::getId, Function.identity()));
            List<BookingCharges> bookingChargesRequestList = new ArrayList<>();
            if (bookingChargesList != null && !bookingChargesList.isEmpty()) {
                for (BookingCharges request : bookingChargesList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingChargesRequestList.add(request);
                }
                responseBookingCharges = saveEntityFromBooking(bookingChargesRequestList, bookingId);
            }
            deleteBookingCharges(hashMap, "CustomerBooking", bookingId);
            return responseBookingCharges;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public void deleteAdditionalPackingByCustomerBookingId(List<Long> bookingChargesIds, Long bookingId) {
        bookingChargesRepository.deleteAdditionalPackingByCustomerBookingId(bookingChargesIds, bookingId);
    }

    @Override
    public void revertSoftDeleteByPackingIdsAndBookingId(List<Long> bookingChargesIds, Long bookingId) {
        bookingChargesRepository.revertSoftDeleteByPackingIdsAndBookingId(bookingChargesIds, bookingId);
    }
}
