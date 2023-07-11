package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToClass;

@Repository
@Slf4j
public class BookingCarriageDao implements IBookingCarriageDao {
    @Autowired
    private IBookingCarriageRepository bookingCarriageRepository;

    @Override
    public BookingCarriage save(BookingCarriage bookingCarriage) {
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

    public List<BookingCarriage> updateEntityFromShipment(CommonRequestModel commonRequestModel, Long shipmentId) throws Exception {
        String responseMsg;
        List<BookingCarriage> responseBookingCarriage = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<BookingCarriage>, Pageable> pair = fetchData(listCommonRequest, BookingCarriage.class);
            Page<BookingCarriage> bookingCarriages = findAll(pair.getLeft(), pair.getRight());
            Map<Long, BookingCarriage> hashMap = bookingCarriages.stream()
                    .collect(Collectors.toMap(BookingCarriage::getId, Function.identity()));
            List<BookingCarriageRequest> bookingCarriagesRequestList = new ArrayList<>();
            List<BookingCarriageRequest> requestList = (List<BookingCarriageRequest>) commonRequestModel.getDataList();
            if (requestList != null && requestList.size() != 0) {
                for (BookingCarriageRequest request : requestList) {
                    Long id = request.getId();
                    request.setShipmentId(shipmentId);
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    bookingCarriagesRequestList.add(request);
                }
                responseBookingCarriage = saveBookingCarriage(bookingCarriagesRequestList);
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

    private List<BookingCarriage> saveBookingCarriage(List<BookingCarriageRequest> bookingCarriages) {
        List<BookingCarriage> res = new ArrayList<>();
        for(BookingCarriageRequest req : bookingCarriages){
            BookingCarriage saveEntity = convertToClass(req, BookingCarriage.class);
            if(req.getId() != null){
                long id = req.getId();
                Optional<BookingCarriage> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Booking Carriage is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            saveEntity = save(saveEntity);
            res.add(saveEntity);
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
}
