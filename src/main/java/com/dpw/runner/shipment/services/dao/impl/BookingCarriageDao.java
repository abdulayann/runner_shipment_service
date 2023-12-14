package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class BookingCarriageDao implements IBookingCarriageDao {
    @Autowired
    private IBookingCarriageRepository bookingCarriageRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public BookingCarriage save(BookingCarriage bookingCarriage) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(bookingCarriage) , Constants.CARRIAGE, LifecycleHooks.ON_CREATE, false);
        if (! errors.isEmpty())
            throw new ValidationException(errors.toString());
        return bookingCarriageRepository.save(bookingCarriage);
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

    public List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId) throws Exception {
        String responseMsg;
        List<BookingCarriage> responseBookingCarriage = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<BookingCarriage>, Pageable> pair = fetchData(listCommonRequest, BookingCarriage.class);
            Page<BookingCarriage> bookingCarriages = findAll(pair.getLeft(), pair.getRight());
            Map<Long, BookingCarriage> hashMap = bookingCarriages.stream()
                    .collect(Collectors.toMap(BookingCarriage::getId, Function.identity()));
            List<BookingCarriage> bookingCarriagesRequestList = new ArrayList<>();
            if (bookingCarriageList != null && bookingCarriageList.size() != 0) {
                for (BookingCarriage request : bookingCarriageList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingCarriagesRequestList.add(request);
                }
                responseBookingCarriage = saveEntityFromShipment(bookingCarriagesRequestList, shipmentId);
            }
            deleteBookingCarriage(hashMap);
            return responseBookingCarriage;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<BookingCarriage> saveEntityFromShipment(List<BookingCarriage> bookingCarriages, Long shipmentId) {
        List<BookingCarriage> res = new ArrayList<>();
        for(BookingCarriage req : bookingCarriages){
            if(req.getId() != null){
                long id = req.getId();
                Optional<BookingCarriage> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
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

    private void deleteBookingCarriage(Map<Long, BookingCarriage> hashMap) {
        String responseMsg;
        try {
            hashMap.values().forEach(this::delete);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    public List<BookingCarriage> updateEntityFromShipment(List<BookingCarriage> bookingCarriageList, Long shipmentId, List<BookingCarriage> oldEntityList) throws Exception {
        String responseMsg;
        List<BookingCarriage> responseBookingCarriage = new ArrayList<>();
        Map<UUID, BookingCarriage> bookingMap = new HashMap<>();
        if(oldEntityList != null && oldEntityList.size() > 0) {
            for (BookingCarriage entity:
                    oldEntityList) {
                bookingMap.put(entity.getGuid(), entity);
            }
        }
        try {

            BookingCarriage oldEntity;
            List<BookingCarriage> bookingCarriagesRequestList = new ArrayList<>();
            if (bookingCarriageList != null && bookingCarriageList.size() != 0) {
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

            deleteBookingCarriage(hashMap);
            return responseBookingCarriage;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
