package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

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
        BookingCarriage bookingCarriage = convertRequestToEntity(request);
        bookingCarriage = bookingCarriageDao.save(bookingCarriage);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(bookingCarriage));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        BookingCarriageRequest request = (BookingCarriageRequest) commonRequestModel.getData();
        long id =request.getId();
        Optional<BookingCarriage> oldEntity = bookingCarriageDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Booking Carriage is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        BookingCarriage bookingCarriage = convertRequestToEntity(request);
        bookingCarriage.setId(oldEntity.get().getId());
        bookingCarriage = bookingCarriageDao.save(bookingCarriage);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(bookingCarriage));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            // construct specifications for filter request
            Pair<Specification<BookingCarriage>, Pageable> tuple = fetchData(request, BookingCarriage.class, tableNames);
            Page<BookingCarriage> bookingCarriagePage  = bookingCarriageDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(bookingCarriagePage.getContent()),
                    bookingCarriagePage.getTotalPages(),
                    bookingCarriagePage.getTotalElements());
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

            BookingCarriageResponse response = convertEntityToDto(bookingCarriage.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private static BookingCarriageResponse convertEntityToDto(BookingCarriage bookingCarriage) {
        BookingCarriageResponse response = new BookingCarriageResponse();
        response.setId(bookingCarriage.getId());
        response.setGuid(bookingCarriage.getGuid());
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

    private static List<IRunnerResponse> convertEntityListToDtoList(List<BookingCarriage> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(bookingCarriage -> {
            responseList.add(convertEntityToDto(bookingCarriage));
        });
        return responseList;
    }

    public static BookingCarriage convertRequestToEntity(BookingCarriageRequest request) {
        BookingCarriage bookingCarriage = new BookingCarriage();
        bookingCarriage.setGuid(UUID.randomUUID());
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
    private static Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("id", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Integer.class).build()),
            Map.entry("polId", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Integer.class).build()),
            Map.entry("podId", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Integer.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Date.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Date.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(String.class).build()),
            Map.entry("carriageType", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(String.class).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(String.class).build()),
            Map.entry("vesselId", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Integer.class).build()),
            Map.entry("bookingId", RunnerEntityMapping.builder().tableName("BookingCarriage").dataType(Integer.class).build())

    );
}
