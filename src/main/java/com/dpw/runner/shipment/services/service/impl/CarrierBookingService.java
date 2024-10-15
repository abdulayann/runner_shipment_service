package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
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

import java.lang.reflect.InvocationTargetException;
import java.security.SecureRandom;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class CarrierBookingService implements ICarrierBookingService {
    ExecutorService executorService = Executors.newFixedThreadPool(10);
    private static final Random rnd = new SecureRandom();

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ICarrierBookingDao carrierBookingDao;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IBookingPaymentDao bookingPaymentDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private MasterDataUtils masterDataUtils;

    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("bookingNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(String.class).fieldName("bookingNumber").isContainsText(true).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(BookingStatus.class).fieldName("status").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CARRIER_BOOKING).dataType(String.class).fieldName("createdBy").build())
    );

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException {
        CarrierBookingRequest request = (CarrierBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Carrier Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        CarrierBooking carrierBooking = jsonHelper.convertValue(request, CarrierBooking.class);

        carrierBooking = carrierBookingDao.save(carrierBooking);
        Long bookingId = carrierBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            carrierBooking.setPackingList(packingDao.saveEntityFromCarrierBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
        if (referenceNumbersRequest != null)
            carrierBooking.setReferenceNumbersList(referenceNumbersDao.saveEntityFromCarrierBooking(commonUtils.convertToEntityList(referenceNumbersRequest, ReferenceNumbers.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            carrierBooking.setContainersList(containers);
        }

        List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
        if (bookingCarriageRequest != null) {
            List<BookingCarriage> bookingCarriages = bookingCarriageDao.saveEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingCarriageRequest, BookingCarriage.class), bookingId);
            carrierBooking.setBookingCarriagesList(bookingCarriages);
        }

        List<BookingPaymentRequest> bookingPaymentRequest = request.getBookingPaymentsList();
        if (bookingPaymentRequest!=null){
            List<BookingPayment> bookingPayments = bookingPaymentDao.saveEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingPaymentRequest, BookingPayment.class), bookingId);
            carrierBooking.setBookingPaymentsList(bookingPayments);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(carrierBooking, CarrierBookingResponse.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || request.getId() == null) {
                log.debug("Request is empty for Carrier Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            long id = request.getId();
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(id);
            if (carrierBooking.isEmpty()) {
                log.debug(CarrierBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            carrierBookingDao.delete(carrierBooking.get());
            log.info("Deleted Booking details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(request, CarrierBooking.class, tableNames);
            Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Carrier Booking list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(carrierBookingPage.getContent()),
                    carrierBookingPage.getTotalPages(),
                    carrierBookingPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CarrierBooking> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(carrierBooking -> {
            CarrierBookingResponse response = modelMapper.map(carrierBooking, CarrierBookingResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return responseList;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CarrierBookingRequest request = (CarrierBookingRequest) commonRequestModel.getData();
        if (request == null || request.getId() == null) {
            log.error("Request is empty for Carrier Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        long id = request.getId();
        Optional<CarrierBooking> oldEntity = carrierBookingDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        CarrierBooking carrierBooking = jsonHelper.convertValue(request, CarrierBooking.class);
        carrierBooking.setCreatedAt(oldEntity.get().getCreatedAt());
        carrierBooking.setCreatedBy(oldEntity.get().getCreatedBy());


        carrierBooking = carrierBookingDao.save(carrierBooking);
        Long bookingId = carrierBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            carrierBooking.setPackingList(packingDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));


        List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
        if (referenceNumbersRequest != null)
            carrierBooking.setReferenceNumbersList(referenceNumbersDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(referenceNumbersRequest, ReferenceNumbers.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            carrierBooking.setContainersList(containers);
        }

        List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
        if (bookingCarriageRequest != null) {
            List<BookingCarriage> bookingCarriages = bookingCarriageDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingCarriageRequest, BookingCarriage.class), bookingId);
            carrierBooking.setBookingCarriagesList(bookingCarriages);
        }

        List<BookingPaymentRequest> bookingPaymentRequest = request.getBookingPaymentsList();
        if (bookingPaymentRequest!=null){
            List<BookingPayment> bookingPayments = bookingPaymentDao.updateEntityFromCarrierBooking(commonUtils.convertToEntityList(bookingPaymentRequest, BookingPayment.class), bookingId);
            carrierBooking.setBookingPaymentsList(bookingPayments);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(carrierBooking, CarrierBooking.class));
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<CarrierBooking> carrierBooking;
            if(id != null) {
                carrierBooking = carrierBookingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                carrierBooking = carrierBookingDao.findByGuid(guid);
            }

            if (carrierBooking.isEmpty()) {
                log.debug(CarrierBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            double current = System.currentTimeMillis();
            log.info("Carrier Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch booking details from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking.get(), CarrierBookingResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
//            createCustomerBookingResponse(carrierBooking.get(), carrierBookingResponse);
            return ResponseHelper.buildSuccessResponse(carrierBookingResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

}
