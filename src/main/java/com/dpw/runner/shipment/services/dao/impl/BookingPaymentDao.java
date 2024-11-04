package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingPaymentDao;
import com.dpw.runner.shipment.services.entity.BookingPayment;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingPaymentRepository;
import com.dpw.runner.shipment.services.service.impl.AuditLogService;
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
public class BookingPaymentDao implements IBookingPaymentDao {

    public static final String BOOKING_PAYMENT_IS_NULL_FOR_ID_MSG = "Booking Payment is null for Id {}";
    @Autowired
    private IBookingPaymentRepository bookingPaymentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;


    @Override
    public BookingPayment save(BookingPayment bookingpayment) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingpayment) , Constants.BOOKING_PAYMENT, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (bookingpayment.getId() != null) {
            Optional<BookingPayment> oldEntity = findById(bookingpayment.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Carrier Booking is null for Id {}", bookingpayment.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
        }
        return bookingPaymentRepository.save(bookingpayment);
    }

    @Override
    public Page<BookingPayment> findAll(Specification<BookingPayment> spec, Pageable pageable) {
        return bookingPaymentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<BookingPayment> findById(Long id) {
        return bookingPaymentRepository.findById(id);
    }

    @Override
    public void delete(BookingPayment bookingpayment) {
        bookingPaymentRepository.delete(bookingpayment);
    }

    public List<BookingPayment> saveAll(List<BookingPayment> bookingPaymentList) {
        for(var bookingPayment : bookingPaymentList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingPayment), Constants.BOOKING_PAYMENT, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
        }
        return bookingPaymentRepository.saveAll(bookingPaymentList);
    }

    public List<BookingPayment> updateEntityFromCarrierBooking(List<BookingPayment> bookingPaymentsList, Long carrierBookingId) throws RunnerException {
        String responseMsg;
        List<BookingPayment> responseBookingPayments = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("carrierBookingId", carrierBookingId, "=");
            Pair<Specification<BookingPayment>, Pageable> pair = fetchData(listCommonRequest, BookingPayment.class);
            Page<BookingPayment> bookingPayments = findAll(pair.getLeft(), pair.getRight());
            Map<Long, BookingPayment> hashMap = bookingPayments.stream().collect(Collectors.toMap(BookingPayment::getId, Function.identity()));
            Map<Long, BookingPayment> hashMapCopy = new HashMap<>(hashMap);
            List<BookingPayment> bookingPaymentRequestList = new ArrayList<>();
            if (bookingPaymentsList != null && !bookingPaymentsList.isEmpty()) {
                for (BookingPayment request : bookingPaymentsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingPaymentRequestList.add(request);
                }
                responseBookingPayments = saveEntityFromCarrierBooking(bookingPaymentRequestList, carrierBookingId, hashMapCopy);
            }
            deleteBookingPayments(hashMap, "CarrierBooking", carrierBookingId);
            return responseBookingPayments;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<BookingPayment> saveEntityFromCarrierBooking(List<BookingPayment> bookingPayments, Long carrierBookingId,  Map<Long, BookingPayment> oldEntityMap) {
        List<BookingPayment> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (BookingPayment req : bookingPayments) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug(BOOKING_PAYMENT_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setCarrierBookingId(carrierBookingId);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(oldEntityJsonStringMap.containsKey(req.getId())){
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, BookingPayment.class) : null)
                                .parent(CarrierBooking.class.getSimpleName())
                                .parentId(carrierBookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    void deleteBookingPayments(Map<Long, BookingPayment> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(bookingPayment -> {
                String json = jsonHelper.convertToJson(bookingPayment);
                delete(bookingPayment);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, BookingPayment.class))
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
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }
}
