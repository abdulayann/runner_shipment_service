package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class BookingCarriageService implements IBookingCarriageService {
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {
        BookingCarriageRequest request = null;
        request = (BookingCarriageRequest) commonRequestModel.getData();
        // TODO- implement validator
        BookingCarriage bookingCarriage = mapToEntityFromRequest(request);
        bookingCarriage = bookingCarriageDao.save(bookingCarriage);
        return ResponseHelper.buildSuccessResponse(convertToResponse(bookingCarriage, null));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        BookingCarriageRequest request = (BookingCarriageRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id =request.getId();
        Optional<BookingCarriage> oldEntity = bookingCarriageDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Booking Carriage is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        BookingCarriage bookingCarriage = mapToEntityFromRequest(request);
        bookingCarriage.setId(oldEntity.get().getId());
        bookingCarriage = bookingCarriageDao.save(bookingCarriage);
        return ResponseHelper.buildSuccessResponse(convertToResponse(bookingCarriage, null));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            List<BookingCarriage> bookingCarriageList = bookingCarriageDao.findAll();

            return ResponseHelper.buildListSuccessResponse(convertListResponse(bookingCarriageList, null), request.getPageNo(), bookingCarriageList.size());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
           // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<BookingCarriage> bookingCarriage = bookingCarriageDao.findById(id);
            if(!bookingCarriage.isPresent()) {
                log.debug("Booking Carriage is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            bookingCarriageDao.delete(bookingCarriage.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id =request.getId();
            Optional<BookingCarriage> bookingCarriage = bookingCarriageDao.findById(id);
            if(!bookingCarriage.isPresent()) {
                log.debug("Booking Carriage is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            BookingCarriageResponse response = convertToResponse(bookingCarriage.get(), null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private static BookingCarriageResponse convertToResponse(BookingCarriage bookingCarriage, BookingCarriageResponse response) {
        if(response == null) {
            response = new BookingCarriageResponse();
        }
        response.setId(bookingCarriage.getId());
        response.setBookingId(bookingCarriage.getBookingId());
        response.setShipmentId(bookingCarriage.getShipmentId());
        response.setVesselId(bookingCarriage.getVesselId());
        response.setPolId(bookingCarriage.getPolId());
        response.setPodId(bookingCarriage.getPodId());
        response.setEta(bookingCarriage.getEta());
        response.setEtd(bookingCarriage.getEtd());
        response.setVessel(bookingCarriage.getVessel());
        response.setVoyage(bookingCarriage.getVoyage());
        response.setCarriageType(bookingCarriage.getCarriageType());
        response.setCarriageMode(bookingCarriage.getCarriageMode());
        return response;
    }

    private static List<IRunnerResponse> convertListResponse(List<BookingCarriage> lst, Map<String, String> locationMap) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(bookingCarriage -> {
            responseList.add(convertToResponse(bookingCarriage, null));
        });
        return responseList;
    }

    public static BookingCarriage mapToEntityFromRequest(BookingCarriageRequest request) {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriage.setBookingId(request.getBookingId());
        bookingCarriage.setShipmentId(request.getShipmentId());
        bookingCarriage.setVesselId(request.getVesselId());
        bookingCarriage.setPolId(request.getPolId());
        bookingCarriage.setPodId(request.getPodId());
        bookingCarriage.setEta(request.getEta());
        bookingCarriage.setEtd(request.getEtd());
        bookingCarriage.setVessel(request.getVessel());
        bookingCarriage.setVoyage(request.getVoyage());
        bookingCarriage.setCarriageType(request.getCarriageType());
        bookingCarriage.setCarriageMode(request.getCarriageMode());
        return bookingCarriage;
    }
}
