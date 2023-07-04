package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageDao;
import com.dpw.runner.shipment.services.service.interfaces.IBookingCarriageService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class BookingCarriageService implements IBookingCarriageService {
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private ModelMapper modelMapper;

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
            Pair<Specification<BookingCarriage>, Pageable> tuple = fetchData(request, BookingCarriage.class);
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

    public ResponseEntity<RunnerResponse<BookingCarriageResponse>> bookingCarriagesUpdate(List<BookingCarriageRequest> requestList, Long shipmentId)
    {
        // TODO- Handle Transactions here
        ResponseEntity<RunnerListResponse<BookingCarriageResponse>> existingList = null;
        ListCommonRequest pageable = new ListCommonRequest();
        pageable.setPageNo(0);
        pageable.setSortRequest(SortRequest.builder()
                .fieldName("deliveryMode")
                .order("DESC")
                .build());
        pageable.setFilterCriteria(Arrays.asList(
                FilterCriteria.builder()
                        .innerFilter(Arrays.asList(
                                FilterCriteria.builder()
                                        .criteria(Criteria.builder()
                                                .fieldName("ShipmentId")
                                                .operator("=")
                                                .value(shipmentId)
                                                .build()).build())).build()));
        existingList = (ResponseEntity<RunnerListResponse<BookingCarriageResponse>>) list(CommonRequestModel.buildRequest(pageable));
        modelMapper.map(existingList.getBody().toString(), BookingCarriageResponse.class);
        // TODO- fetch based on shipmentId using service
        HashSet<Long> existingIds = new HashSet<>( existingList.getBody().stream().map(BookingCarriage::getId).collect(Collectors.toList()) );
        List<BookingCarriageRequest> newBookingCarriages = new ArrayList<>();
        List<BookingCarriageRequest> updateBookingCarriages = new ArrayList<>();
        for(BookingCarriageRequest request: requestList)
        {
            Long id = request.getId();
            if(id != null){
                existingIds.remove(id);
                updateBookingCarriages.add(request);
            }
            else
                newBookingCarriages.add(request);
        }
        createBookingCarriages(newBookingCarriages);
        updateBookingCarriages(updateBookingCarriages);
        for(Long id: existingIds)
        {
            delete(CommonRequestModel.buildRequest(id));
        }
        return null;
    }

    private ResponseEntity<RunnerResponse<BookingCarriageResponse>> updateBookingCarriages(List<BookingCarriageRequest> updateBookingCarriages)
    {
        for(BookingCarriageRequest request: updateBookingCarriages)
        {
            String responseMessage;
            try {
                update(CommonRequestModel.buildRequest(request));
            } catch (Exception e) {
                responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
                return (ResponseEntity<RunnerResponse<BookingCarriageResponse>>) ResponseHelper.buildFailedResponse(responseMessage);
            }
        }
        return null;
    }

    private ResponseEntity<RunnerResponse<BookingCarriageResponse>> createBookingCarriages(List<BookingCarriageRequest> newBookingCarriages)
    {
        for(BookingCarriageRequest request: newBookingCarriages)
        {
            String responseMessage;
            try {
                create(CommonRequestModel.buildRequest(request));
            } catch (Exception e) {
                responseMessage = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
                log.error(responseMessage, e);
            }
        }
        return null;
    }

    private BookingCarriageResponse convertEntityToDto(BookingCarriage bookingCarriage) {
        return modelMapper.map(bookingCarriage, BookingCarriageResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<BookingCarriage> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(bookingCarriage -> {
            responseList.add(convertEntityToDto(bookingCarriage));
        });
        return responseList;
    }

    public BookingCarriage convertRequestToEntity(BookingCarriageRequest request) {
        return modelMapper.map(request, BookingCarriage.class);
    }
}
