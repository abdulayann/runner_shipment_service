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
    private AuditLogService auditLogService;


    @Override
    public BookingPayment save(BookingPayment bookingpayment) {

        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingpayment) , Constants.BOOKING_PAYMENT, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if (bookingpayment.getId() != null) {
            Optional<BookingPayment> oldEntity = findById(bookingpayment.getId());
            if (oldEntity.isEmpty()) {
                log.debug("Customer Booking is null for Id {}", bookingpayment.getId());
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


    public List<BookingPayment> saveEntityFromCarrierBooking(List<BookingPayment> bookingPayments, Long carrierBookingId) {
        List<BookingPayment> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("carrierBookingId", carrierBookingId, "=");
        Pair<Specification<BookingPayment>, Pageable> pair = fetchData(listCommonRequest, BookingPayment.class);
        Page<BookingPayment> bookingPaymentPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, BookingPayment> hashMap = bookingPaymentPage.stream().collect(Collectors.toMap(BookingPayment::getId, Function.identity()));
        for (BookingPayment req : bookingPayments) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug(BOOKING_PAYMENT_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                operation = DBOperationType.UPDATE.name();
            }
            req.setCarrierBookingId(carrierBookingId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
                                .parent(CarrierBooking.class.getSimpleName())
                                .parentId(carrierBookingId)
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

    public List<BookingPayment> updateEntityFromCarrierBooking(List<BookingPayment> bookingPaymentsList, Long carrierBookingId) throws RunnerException {
        String responseMsg;
        List<BookingPayment> responseBookingPayments = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("carrierBookingId", carrierBookingId, "=");
            Pair<Specification<BookingPayment>, Pageable> pair = fetchData(listCommonRequest, BookingPayment.class);
            Page<BookingPayment> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, BookingPayment> hashMap = packings.stream().collect(Collectors.toMap(BookingPayment::getId, Function.identity()));
            List<BookingPayment> bookingPaymentRequestList = new ArrayList<>();
            if (bookingPaymentsList != null && !bookingPaymentsList.isEmpty()) {
                for (BookingPayment request : bookingPaymentsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingPaymentRequestList.add(request);
                }
                responseBookingPayments = saveEntityFromCarrierBooking(bookingPaymentRequestList, carrierBookingId);
            }
            deleteBookingPayments(hashMap, "CarrierBooking", carrierBookingId);
            return responseBookingPayments;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }


    private void deleteBookingPayments(Map<Long, BookingPayment> hashMap, String entity, Long entityId) {
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
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }
}
